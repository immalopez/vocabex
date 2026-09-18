from typing import List, Set, Any

from pandas.core.common import flatten

from vocabex.preprocess import locate_terms_in_docs


def collect_context_words_by_terms(terms_locs, docs, window) -> List[List[str]]:
    return [collect_context_words_for_term(term_locs, docs, window)
            for term_locs in terms_locs]


def collect_context_words_for_term(
        term_locs,
        docs,
        window,
        should_flatten=True,
        include_match=False
) -> list[Any] | list[list[Any]]:
    from string import punctuation

    contexts = []
    for doc_index, locs in enumerate(term_locs):
        doc = docs[doc_index]
        contexts.append(
            context_words_for_doc_and_locs(
                doc,
                locs,
                window,
                should_flatten,
                include_match
            )
        )

    if should_flatten:
        flattened = [word for words in contexts for word in words]
        sanitized = [word.lower()
                     for word in flattened
                     if word not in punctuation + r'¡¿–—']
        # remove duplicates, if you want to maintain the element order
        return list(dict.fromkeys(sanitized))
    else:
        return contexts


def context_words_for_doc_and_locs(
        doc,
        locs,
        window,
        should_flatten=True,
        include_match=False
):
    contexts = []
    for sent_loc in locs:
        sent_index = sent_loc[0]
        start, end = sent_loc[1]
        sent = doc[sent_index]

        # limit indices to sentence bounds using `min` and `max`
        # to safely use with list slicing
        # since out-of-bounds slicing would return empty list ([])
        slice_before = slice(max(0, start - window), start)
        slice_after = slice(end, min(end + window, len(sent)))
        if include_match:
            context = sent[slice_before] + sent[start:end] + sent[slice_after]
        else:
            context = sent[slice_before] + sent[slice_after]
        contexts.append(context)

    if should_flatten:
        return [word for context in contexts for word in context]
    else:
        return contexts


def locate_ctx_terms_in_docs(ctx_words_by_term, texts):
    ctx_terms_flat = list(flatten(ctx_words_by_term))
    ctx_terms = [[word] for word in ctx_terms_flat]
    ctx_locs_flat = locate_terms_in_docs(ctx_terms, texts)
    word_to_locs_mapping = dict(zip(ctx_terms_flat, ctx_locs_flat))
    ctxs_locs = ctxs_locs_by_term(word_to_locs_mapping, ctx_words_by_term)
    return ctxs_locs


def ctxs_locs_by_term(term_loc_dict, ctx_words_by_term):
    return [[term_loc_dict[w] for w in ws]
            for ws in ctx_words_by_term]


def transpose_ctx_terms_to_docs_locations(
        ctxs_locs_by_terms,
        terms_count,
        docs_count,
):
    ctxs_locs_by_docs = [{} for _ in range(docs_count)]
    for t_index, t in enumerate(ctxs_locs_by_terms):
        for c_key, c_value in t.items():
            for d_index, d in enumerate(c_value):
                if len(d):
                    ctxs_locs_by_docs[d_index].setdefault(
                        c_key,  # get value for key
                        [[] for _ in range(terms_count)]  # value if missing
                    )[t_index] = d
    # Note: d = doc, c = context word, t = term
    return ctxs_locs_by_docs


def to_list_of_dicts(keys, values):
    return [dict(zip(key, value)) for key, value in zip(keys, values)]
