from __future__ import annotations

import pandas as pd
from codetiming import Timer
from lexicalrichness import LexicalRichness
from pandas.core.common import flatten

from vocabex.constants import (
    COL_STANZA_DOC,
)
from vocabex.preprocess import (
    run,
    text_analysis_pipeline,
)


def get_lexical_richness(text):
    """
    Calculates the lexical richness of the text.
    """
    if not text or text == "":
        return {
            "msttr": 0,
            "mattr": 0,
            "ttr": 0,
            "rttr": 0,
            "cttr": 0,
            "mtld": 0,
            "hdd": 0,
            "Herdan": 0,
            "Summer": 0,
            "Dugast": 0,
            "Maas": 0,
        }
    else:
        lex = LexicalRichness(text)
        return {
            "msttr": lex.msttr(segment_window=min(100, lex.words - 1)),
            "mattr": lex.mattr(window_size=min(100, lex.words - 1)),
            "ttr": lex.ttr,
            "rttr": lex.rttr,
            "cttr": lex.cttr,
            "mtld": lex.mtld(threshold=0.72),
            "hdd": lex.hdd(draws=min(42, lex.words)),
            "Herdan": lex.Herdan,
            "Summer": lex.Summer,
            "Dugast": lex.Dugast if lex.words != lex.terms else 0,
            "Maas": lex.Maas,
        }


def calc_stats_for_group(group_name: str, group_df: pd.DataFrame, max_docs: int):
    timer_text = "{name}: {:0.0f} seconds"
    texts_df = group_df

    if max_docs:
        texts_df = texts_df[:max_docs].copy()

    with Timer(name="Preprocess", text=timer_text):
        texts_df = run(texts_df, text_analysis_pipeline)

    with Timer(name="UPOS", text=timer_text):
        stanza_docs = texts_df[COL_STANZA_DOC]
        stats_per_doc = [calc_stats_for_stanza_doc(doc) for doc in stanza_docs]
        return stats_per_doc


def calc_stats_for_stanza_doc(doc):
    all_words = [word for sent in doc.sentences for word in sent.words]
    all_words_count = len(all_words)

    upos_dict = {"count": 0, "vals": {}}
    for w in all_words:
        # count general words
        upos_dict["count"] += 1

        # init and count the current upos
        curr_upos = upos_dict["vals"].setdefault(w.upos, {"count": 0})
        curr_upos["count"] += 1

        if w.feats is not None:
            # init feats container
            curr_upos.setdefault("vals", {})
            feats = curr_upos["vals"]

            for f in w.feats.split("|"):
                k, v = f.split("=")

                # init feat
                feat = feats.setdefault(k, {"count": 0, "vals": {}})
                feat["count"] += 1

                # init and count value
                feat["vals"].setdefault(v, 0)
                feat["vals"][v] += 1

    deprel_dict = {}
    for w in all_words:
        key = w.deprel
        deprel_dict.setdefault(key, 0)
        deprel_dict[key] += 1

    return {"upos": upos_dict, "deprel": deprel_dict}


def find(data, keys):
    rv = data
    for key in keys:
        rv = rv.get(key, {})
    return rv if rv != {} else 0


def make_key_str(keys):
    return "-".join([key for key in keys if key not in ["vals", "count"]])


def calc_upos_ratios(data):
    new_data = {}
    for k, v in data.items():
        if k.startswith("deprel") or ("-" not in k):
            continue
        parent_k = "-".join(k.split("-")[:-1])
        parent_v = data[parent_k]
        k_group = k + " GRP%"
        new_data[k_group] = [x / y if y != 0 else 0 for x, y in zip(v, parent_v)]
        all_k = "".join(k.split("-")[:1])
        all_v = data[all_k]
        k_all = k + " DOC%"
        new_data[k_all] = [x / y if y != 0 else 0 for x, y in zip(v, all_v)]
    return new_data


def calc_deprel_ratios(data):
    new_data = {}
    for k, v in data.items():
        if k.startswith("upos") or ("-" not in k):
            continue
        parent_v = data["deprel"]
        new_data[k + " DOC%"] = [x / y if y != 0 else 0 for x, y in zip(v, parent_v)]
    return new_data


def group_upos_values_by_key(result, next, docs, path):
    for k, v in next.items():
        cur_path = path + [k]
        if isinstance(v, dict):
            group_upos_values_by_key(result, v, docs, cur_path)
        else:
            key = make_key_str(cur_path)
            result[key] = [find(doc, cur_path) for doc in docs]


def collect_stats_keys(result, doc, path):
    for k, v in doc.items():
        cur_path = path + [k]
        if isinstance(v, dict):
            collect_stats_keys(result, v, cur_path)
        else:
            cur_dict = result
            for k1 in cur_path[:-1]:
                cur_dict = cur_dict.setdefault(k1, {})
            last_key = "".join(cur_path[-1:])
            cur_dict[last_key] = 0


def calc_lex_density(row):
    total = row["Total"]
    if total == 0 or total is None:
        return 0
    adj = row["upos-ADJ"] if "upos-ADJ" in row else 0
    adv = row["upos-ADV"] if "upos-ADV" in row else 0
    intj = row["upos-INTJ"] if "upos-INTJ" in row else 0
    noun = row["upos-NOUN"] if "upos-NOUN" in row else 0
    propn = row["upos-PROPN"] if "upos-PROPN" in row else 0
    verb = row["upos-VERB"] if "upos-VERB" in row else 0
    lex_density = (adj + adv + intj + noun + propn + verb) / total
    return lex_density
