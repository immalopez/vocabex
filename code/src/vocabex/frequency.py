def count_terms(terms_locs):
    return [sum(1 for sents in term_locs for _ in sents)
            for term_locs in terms_locs]


def count_doc_terms(docs_locs, term_indices):
    return [
        sum(len(doc_locs[index]) for index in term_indices)
        for doc_locs in docs_locs
    ]
