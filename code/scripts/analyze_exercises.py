import pickle
from pathlib import Path

import openpyxl
import pandas as pd
from codetiming import Timer
from pandas.core.common import flatten

from vocabex.constants import COL_LEMMA, COL_LEVEL, COL_STANZA_DOC
from vocabex.frequency import count_doc_terms
from vocabex.preprocess import (
    locate_terms_in_docs,
    post_process_lemmatization,
    run,
    text_analysis_pipeline,
    vocabulary_pipeline,
)
from vocabex.stats import (
    calc_deprel_ratios,
    calc_lex_density,
    calc_stats_for_stanza_doc,
    calc_upos_ratios,
    collect_stats_keys,
    get_lexical_richness,
    group_upos_values_by_key,
)
from vocabex.tfidf import calc_mean_doc_idfs
from vocabex.tree import texts_tree_props_pipeline


def should_use_cache(cache_path):
    """Check if cache is valid"""
    return cache_path.exists()


def get_cache_path(stage_name):
    """Generate cache file path for a given stage"""
    cache_dir = Path("./cache")
    cache_dir.mkdir(exist_ok=True)
    return cache_dir / f"{stage_name.lower().replace(' ', '_')}_cache.pkl"


def load_cache(cache_path):
    """Load cached DataFrames"""
    print(f"Loading cache from {cache_path}")
    with open(cache_path, "rb") as f:
        return pickle.load(f)


def save_cache(cache_path, data):
    """Save DataFrames to cache"""
    print(f"Saving cache to {cache_path}")
    with open(cache_path, "wb") as f:
        pickle.dump(data, f)


def main():
    print()
    print("ANALYZE DOCS START")
    print("---")

    timer_text = "{name}: {:0.0f} seconds"

    with Timer(name="Load data", text=timer_text, initial_text=True):
        cache_path = get_cache_path("load_data")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            terms_df = cached_data.get("terms_df")
            texts_df = cached_data.get("texts_df")
        else:
            terms_df = pd.read_csv("./resources/terms.csv", sep=";")
            texts_df = pd.read_csv("./resources/texts.csv", sep=",")
            texts_df = texts_df.rename(columns={"text_file": "Text file"})
            texts_df["Text file"] = (
                "./data/processed/exercises/" + texts_df["Text file"]
            )
            save_cache(cache_path, {"terms_df": terms_df, "texts_df": texts_df})

    with Timer(name="Preprocess", text=timer_text, initial_text=True):
        cache_path = get_cache_path("preprocess")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            terms_df = cached_data.get("terms_df")
            texts_df = cached_data.get("texts_df")
            texts = cached_data.get("texts")
            storage = cached_data.get("storage")
        else:
            texts_df = run(texts_df, text_analysis_pipeline)
            terms_df = run(terms_df, vocabulary_pipeline)
            terms_df = post_process_lemmatization(terms_df)
            texts = texts_df[COL_LEMMA]
            storage = {"stanza": texts_df[COL_STANZA_DOC], "tree": {}}
            texts_df["Total"] = texts_df["Lemma"].apply(
                lambda x: sum(1 for _ in flatten(x))
            )
            save_cache(
                cache_path,
                {
                    "terms_df": terms_df,
                    "texts_df": texts_df,
                    "texts": texts,
                    "storage": storage,
                },
            )

    with Timer(name="Locate terms", text=timer_text, initial_text=True):
        cache_path = get_cache_path("locate_terms")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            terms = cached_data.get("terms")
            terms_locs = cached_data.get("terms_locs")
        else:
            terms = [term for terms in terms_df[COL_LEMMA] for term in terms]
            terms_locs = locate_terms_in_docs(terms, texts)
            save_cache(cache_path, {"terms": terms, "terms_locs": terms_locs})

    with Timer(name="Frequency", text=timer_text, initial_text=True):
        cache_path = get_cache_path("frequency")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            texts_df = cached_data.get("texts_df")
            docs_locs = cached_data.get("docs_locs")
            levels = cached_data.get("levels")
        else:
            docs_locs = list(zip(*terms_locs))
            levels = terms_df[COL_LEVEL].unique()
            for level in levels:
                term_indices = (
                    terms_df.groupby(COL_LEVEL).get_group(level).index.to_list()
                )
                texts_df[f"Count {level}"] = count_doc_terms(docs_locs, term_indices)
            texts_df["Count"] = count_doc_terms(
                docs_locs,
                term_indices=range(0, len(terms_df)),  # all terms
            )
            for level in levels:
                texts_df[f"Freq {level}"] = (
                    texts_df[f"Count {level}"] / texts_df["Total"]
                )
            texts_df["Freq"] = texts_df["Count"] / texts_df["Total"]
            save_cache(
                cache_path,
                {"texts_df": texts_df, "docs_locs": docs_locs, "levels": levels},
            )

    with Timer(name="Tree", text=timer_text, initial_text=True):
        cache_path = get_cache_path("tree")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            texts_df = cached_data.get("texts_df")
        else:
            texts_df["Tree"] = texts_tree_props_pipeline(storage, docs_locs)

            def split_tree_column(row):
                min_w, max_w, avg_w, min_h, max_h, avg_h = row["Tree"]
                row["tree_min_w"] = min_w
                row["tree_max_w"] = max_w
                row["tree_avg_w"] = avg_w
                row["tree_min_h"] = min_h
                row["tree_max_h"] = max_h
                row["tree_avg_h"] = avg_h
                return row

            texts_df = texts_df.apply(split_tree_column, axis=1)
            save_cache(cache_path, {"texts_df": texts_df})

    with Timer(name="TFIDF", text=timer_text, initial_text=True):
        cache_path = get_cache_path("tfidf")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            texts_df = cached_data.get("texts_df")
        else:
            for level in levels:
                term_indices = (
                    terms_df.groupby(COL_LEVEL).get_group(level).index.to_list()
                )
                texts_df[f"IDF {level}"] = calc_mean_doc_idfs(docs_locs, term_indices)
                texts_df[f"TFIDF {level}"] = (
                    texts_df[f"Freq {level}"] * texts_df[f"IDF {level}"]
                )
                texts_df = texts_df.drop(columns=f"IDF {level}")

            texts_df["IDF"] = calc_mean_doc_idfs(docs_locs, range(0, len(terms_df)))
            texts_df["TFIDF"] = texts_df["Freq"] * texts_df["IDF"]
            texts_df = texts_df.drop(columns="IDF")
            save_cache(cache_path, {"texts_df": texts_df})

    with Timer(name="Lexical Richness", text=timer_text, initial_text=True):
        cache_path = get_cache_path("lexical_richness")
        if should_use_cache(cache_path):
            cached_data = load_cache(cache_path)
            texts_df = cached_data.get("texts_df")
        else:
            lex_by_doc = texts_df["Raw text"].apply(get_lexical_richness)
            lex = {k: [doc[k] for doc in lex_by_doc] for k in lex_by_doc[0]}
            for name, values in lex.items():
                texts_df[name] = values
            save_cache(cache_path, {"texts_df": texts_df})

    with Timer(name="UPOS", text=timer_text, initial_text=True):
        stats_per_doc = [
            calc_stats_for_stanza_doc(doc) for doc in texts_df[COL_STANZA_DOC]
        ]
        stats_keys = {}
        for d in stats_per_doc:
            collect_stats_keys(stats_keys, d, [])
        upos_dict = {}
        group_upos_values_by_key(upos_dict, stats_keys, stats_per_doc, [])
        texts_df = texts_df.join(pd.DataFrame(upos_dict))

        upos_dict["deprel"] = upos_dict["upos"]  # copy totals
        depr_dict_ratios = calc_deprel_ratios(upos_dict)
        texts_df = texts_df.join(pd.DataFrame(depr_dict_ratios))

        upos_dict_ratios = calc_upos_ratios(upos_dict)
        texts_df = texts_df.join(pd.DataFrame(upos_dict_ratios))
        texts_df["Lexical density"] = texts_df.apply(calc_lex_density, axis=1)
        save_cache(cache_path, {"texts_df": texts_df})

    with Timer(name="Export CSV", text=timer_text, initial_text=True):
        columns_to_remove = ["Lemma", "Text", "Raw text", "Stanza Doc", "Tree"]
        export_df = texts_df[
            [col for col in texts_df.columns if col not in columns_to_remove]
        ]
        file_name = "./outputs/exercises_analysis.csv"
        export_df.to_csv(file_name, index=False)
        print("Exported file:", file_name)

    print("Done")


if __name__ == "__main__":
    main()
