##############################################################################
#                            FREQUENCY CALCULATIONS                          #
##############################################################################
import math


def tfidfs_per_doc(terms_locs, docs):
    """
    term_locs
    ---
    A matrix with rows corresponding to terms and columns corresponding
    to docs. If there are 2 terms and 3 docs it would be a 2x3 matrix
    filled with indices of sentences where a term is found.
    Shape: [
        [[(sent_index, (term_start_index, term_end_index))]],
        [[...]],
        ...,
    ]

    docs
    ---
    docs is a list of doc where each
        doc is a list of sent where each
            sent is a list of word
    [
        [['This', 'is', 'a', 'sentence', '.'], ['And', 'this', 'as', 'well']],
        [['Another', 'sentence']]
    ]

    returns
    ---
    a list of TFIDFs per term per doc where each doc is the average of TFIDFs
    of the sentences in a doc since we consider each sentence to be a doc.
    Shape: [
        [0.12, 0.459, 0, ...],
        [...],
        ...,
    ]

    """
    # For each term in the vocabulary:
    result_tfidfs_per_term = []
    for term_locs in terms_locs:

        tfidfs_docs = []
        for doc_idx, doc_sents in enumerate(docs):
            tfidfs_docs.append(0)
            doc_matches = term_locs[doc_idx]

            sents_count = len(doc_sents)
            sents_matched = len(doc_matches)

            if sents_matched > 0:
                idf_doc = math.log10(sents_count / sents_matched)

                tfs_sents = []
                for doc_match in doc_matches:
                    term_start = doc_match[1][0]
                    term_end = doc_match[1][1]
                    term_word_count = term_end - term_start
                    sent_idx = doc_match[0]
                    sent_words = doc_sents[sent_idx]
                    # count multi-word terms as 1
                    # by removing its length and adding 1 instead
                    sent_word_count = (len(sent_words) - term_word_count + 1)
                    # use frequency 1 since we match 1 term per sentence max
                    tf = 1 / sent_word_count
                    tfs_sents.append(tf)

                tfidfs_sents = map(lambda tf: tf * idf_doc, tfs_sents)
                tfidfs_doc_sum = sum(tfidfs_sents)
                tfidf_doc_avg = tfidfs_doc_sum / sents_count
                tfidfs_docs[doc_idx] = tfidf_doc_avg

        # TF-IDF for current term
        result_tfidfs_per_term.append(tfidfs_docs)

    return result_tfidfs_per_term


def calc_mean_doc_idfs(docs_terms_locs, term_indices):
    doc_matches_mask = [
        [1 if len(locs) else 0 for locs in [terms_locs[term_idx]
                                            for term_idx in term_indices]]
        for terms_locs in docs_terms_locs
    ]
    # [1, 1, 0, 1, 1]
    # [0, 0, 1, 1, 0]
    docs_match_counts = [
        sum(doc_matches)
        for doc_matches in doc_matches_mask
    ]
    # [4, 2]
    term_match_counts = [sum(column)
                         for column in zip(*doc_matches_mask)]
    # [1, 1, 1, 2, 1]
    doc_count = len(docs_terms_locs)
    # 2
    term_idfs = [
        math.log10(doc_count / matched) if matched > 0 else 0
        for matched in term_match_counts
    ]
    # [0.3010, 0.3010, 0.3010, 0.0000, 0.3010]
    docs_idfs = [
        [term_idfs[idx] if bit else 0
         for idx, bit in enumerate(doc_mask)]
        for doc_mask in doc_matches_mask
    ]
    # [
    #   [0.3010, 0.3010, 0, 0.0, 0.3010],
    #   [0, 0, 0.3010, 0.0, 0]
    # ]
    doc_avg_idfs = [
        sum(doc_idfs) / docs_match_counts[doc_idx]
        if docs_match_counts[doc_idx] > 0 else 0
        for doc_idx, doc_idfs in enumerate(docs_idfs)
    ]
    # [0.2257, 0.1505]
    return doc_avg_idfs

    # texts_df["IDF"] = calc_doc_avg_idfs(docs_locs)
    # texts_df['TFIDF'] = texts_df["Freq"] * texts_df["IDF"]
    # 0.03099
    # 0.01881
