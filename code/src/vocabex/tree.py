##############################################################################
#                               TREE CALCULATIONS                            #
##############################################################################

import funcy
from anytree import Node, LevelOrderGroupIter


def make_trees_for_terms_docs_sents_term_loc(storage, terms_locs):
    return [make_tree_for_docs_sents_term_loc(storage, docs_sents_term_loc)
            for docs_sents_term_loc in terms_locs]


def make_tree_for_docs_sents_term_loc(storage, docs_sents_term_loc):
    return [[make_tree_for_loc(storage, doc_index, term_loc[0])  # 0 = sent idx
             for term_loc in sents_locs]
            for doc_index, sents_locs in enumerate(docs_sents_term_loc)]


def make_trees_for_doc_contexts(storage, doc_contexts):
    """Flatten term locations for a doc and map to trees.

    Returns
    -------
    list: List[Node]
        a list of tree properties with the mean values of all the unique
        sentences of a document.
    """
    return [
        [make_tree_for_loc(storage, doc_index, sent_idx)
         for sent_idx in {
             loc[0]
             for context in doc.values()
             for term in context
             for loc in term
         }]
        for doc_index, doc in enumerate(doc_contexts)
    ]


def make_trees_for_docs_terms_sents_term_loc(storage, docs_locs):
    return [
        make_tree_for_terms_sents_term_loc(
            storage,
            doc_idx,
            terms_sents_term_loc
        )
        for doc_idx, terms_sents_term_loc in enumerate(docs_locs)
    ]


def make_tree_for_terms_sents_term_loc(
        storage,
        doc_index,
        terms_sents_term_loc
):
    return [[make_tree_for_loc(storage, doc_index, term_loc[0])  # 0 = sent idx
             for term_loc in sents_locs]
            for sents_locs in terms_sents_term_loc]


def make_tree_for_loc(storage, doc_idx, sent_idx) -> Node:
    stanza_docs = storage['stanza']
    sent = stanza_docs[doc_idx].sentences[sent_idx]
    trees = storage['tree']
    key = f'{doc_idx}_{sent_idx}'

    if key in trees:
        return trees[key]

    nodes = {word.id: Node(word.lemma) for word in sent.words}
    for word in sent.words:
        nodes[word.id].parent = nodes[word.head] if word.head > 0 else None
        if word.head == 0:
            root = nodes[word.id]

    storage['tree'][key] = root
    return root


def calculate_tree_props_v2(terms_trees):
    return [calc_mean_tree_props_for_nested_trees(docs_trees) for docs_trees in terms_trees]


def calculate_tree_props_for_doc_contexts(doc_contexts):
    return [calc_mean_tree_props_for_trees(doc) for doc in doc_contexts]


def calc_mean_tree_props_for_nested_trees(
        nested_trees: [[Node]]
) -> (int, int, int, int, int, int):
    """
    Flatten and calculate the min, max, mean of widths and heights of trees.
    """
    return calc_mean_tree_props_for_trees(
        [tree for trees in nested_trees for tree in trees]
    )


def calc_mean_tree_props_for_trees(
        trees: [Node]
) -> (int, int, int, int, int, int):

    min_w, min_h = 1000, 1000
    max_w, max_h = 0, 0
    sum_w, sum_h = 0, 0
    num_nodes = 0

    for tree in trees:
        width, height = get_tree_props_for_sent(tree)

        sum_w += width
        sum_h += height
        num_nodes += 1

        min_w = min(min_w, width)
        max_w = max(max_w, width)
        min_h = min(min_h, height)
        max_h = max(max_h, height)

    avg_w = sum_w / num_nodes if num_nodes > 0 else None
    avg_h = sum_h / num_nodes if num_nodes > 0 else None

    has_result = max_w > 0
    if has_result:
        return min_w, max_w, avg_w, min_h, max_h, avg_h
    else:
        return None, None, None, None, None, None



def get_tree_props_for_sent(sent_tree):
    width = max([len(children)
                 for children in LevelOrderGroupIter(sent_tree)])
    height = sent_tree.height
    return width, height


def calc_mean_doc_context_tree(storage, doc_context_terms_locs):
    pass

##############################################################################
#                                 PIPELINE                                   #
##############################################################################


terms_tree_props_pipeline = funcy.rcompose(
    make_trees_for_terms_docs_sents_term_loc,
    calculate_tree_props_v2
)
texts_tree_props_pipeline = funcy.rcompose(
    make_trees_for_docs_terms_sents_term_loc,
    calculate_tree_props_v2
)
contexts_tree_props_pipeline = funcy.rcompose(
    make_trees_for_doc_contexts,
    calculate_tree_props_for_doc_contexts
)
