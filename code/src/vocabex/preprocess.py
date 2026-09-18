##############################################################################
#                            CORPUS PREPROCESSING                            #
##############################################################################

import stanza as st
from nltk.corpus import stopwords, names

from vocabex import data
from vocabex._typing import *
from vocabex.constants import *
from vocabex.utils import *

# Initialized on demand
nlp_es = None
nlp_es_simple = None

# ============================== PIPELINE STEPS ===============================


def set_column(func, src, dst, args=()):
    """
    Returns a func to be used in a pipeline (i.e. a func that receives and
    returns a DataFrame). Parameters are used to specify on what
    data to work on and which transformation to apply.

    :param dst: Column name to assign in the modified DataFrame.
    :param src: Column name from which to take the data.
    :param func: Transformer func working with the data.
    :param args: Any extra args the transformer might need for its work.
    :return: A preconfigured func with input and output columns and an
    operation to run on the data. The returned func is intended to be used
    in our pipeline (i.e. receives and returns a DatFrame).
    """

    def modify(df):
        # Run `func` only for new / not restored from cache columns.
        if dst not in df.columns:
            df[dst] = func(df[src], *args)
        return df

    return modify


def read_files(file_paths: [str]) -> [str]:
    """
    Read files from the file system. Delegates actual work to `data` module.

    :param file_paths: Paths to open for reading.
    :return: a list of strings with content of the input files.
    """
    return data.read_files(file_paths)


def shrink_text_content(texts) -> [str]:
    # shrink to 10% of the original size (i.e. throw away 90% of the content)
    return [t[: round(len(t) * 0.1)] for t in texts]


def make_stanza_docs_simple(texts):
    global nlp_es_simple

    if nlp_es_simple is None:
        nlp_es_simple = st.Pipeline(lang="es", processors="tokenize,lemma")

    documents = [st.Document([], text=d) for d in texts]
    return nlp_es_simple(documents)


def make_stanza_docs_ner(texts):
    global nlp_es_simple

    if nlp_es_simple is None:
        nlp_es_simple = st.Pipeline(lang="es", processors="tokenize,lemma,ner")

    documents = [st.Document([], text=d) for d in texts]
    return nlp_es_simple(documents)


def make_stanza_docs(texts):
    global nlp_es

    # Initialize only once if not initialized.
    if nlp_es is None:
        nlp_es = st.Pipeline(lang="es", processors="tokenize,mwt,lemma,pos,depparse")

    """Processes a list of strings and returns it as a list of lists (docs)
    of sublists, wherein each sublist is a sentence that in turn contains a
    dictionary per word with the following information: "id" (ID of the word),
    "text" (token), "lemma" (lemmatized token), "upos" (universal POS), "xpos"
    (treebank-specific POS), "feats" (universal morphological features of the
    word), "start_char" (starting character) & "end_char" (ending one)."""

    # An inefficient way to process texts one by one.
    # Left here for reference.
    # return [nlp_es(text) for text in texts]

    # Wrap each document with a stanza.Document object
    documents = [st.Document([], text=d) for d in texts]
    # Call the neural pipeline on this list of documents
    return nlp_es(documents)


def get_fields(documents, key):
    """Returns a list of lists (sentences) for each document with the specified
    property (id, text, lemma, upos, xpos, feats, start_char, & end_char)."""
    return [d.get(key, True) for d in documents]


def normalize(column):
    return [[[word.lower() for word in sent] for sent in texts] for texts in column]


def remove_stopwords(column):
    blacklist = (
        stopwords.words("spanish")
        + names.words("male.txt")
        + names.words("female.txt")
        + [
            "pedro",
            "leo",
            "leopoldo",
            "leotoldo",
            "hans",
            "vázquez",
            "ivanovna",
            "luis",
            "mary",
            "richard",
            "santiago",
            "505",
        ]
    )
    return [
        [[word for word in sent if word not in blacklist] for sent in texts]
        for texts in column
    ]


def locate_terms_in_docs(terms, docs):
    """
    terms
    ---
    terms is a list of lists, where each
        term is a list of words to support multi-word terms.
    [
        ['multi', 'word', 'term'],  # term 1
        ['single1'],                # term 2
        ['single2'],                # term 3
    ]

    docs
    ---
    docs is a list, where each
        doc is a list of sents, where each
            sent is a list of words.
    [
        [['This', 'is', 'a', 'sentence', '.'], ['And', 'this', 'as', 'well']],
        [['Another', 'sentence']]
    ]

    returns
    ---
    A matrix with rows corresponding to input terms and columns corresponding
    to input docs. If there are 2 terms and 3 docs it will create 2x3 matrix
    filled with indices of sentences where a term is found.
    Shape: [
        [[(sent_index, (term_start_index, term_end_index))]],
        [[...]],
        ...,
    ]
    """
    result_terms = []
    for term in terms:
        result_docs = []
        for doc_sents in docs:
            result_sents = []
            for sent_index, sent in enumerate(doc_sents):
                loc = first_occurrence_of_term_in_sent(term, sent)
                if loc:
                    result_sents.append((sent_index, loc))
            result_docs.append(result_sents)
        result_terms.append(result_docs)
    return result_terms


def vocabs_locations_in_texts(
    vocabs: DataFrame, texts: DataFrame, column: str, is_context: bool = False
) -> (str, [(int, (int, int))]):
    if is_context:
        # When processing context words, we restructure the list of words
        # to mimic the structure of the non-context words to re-use the logic.
        rows = []
        for context_row in vocabs[column + " " + CONTEXT]:
            current_row = []
            for word in context_row:
                current_row.append([word])
            rows.append(current_row)
        vocab_series = rows
    else:
        vocab_series = vocabs[COL_LEMMA]

    doc_series = texts[COL_STANZA_DOC]
    loc_phrases = []

    for vocab_row in vocab_series:
        loc_docs = []

        # Idea: cache sentences instead of re-creating them for every doc
        # OR use the 'lemma' column of the texts DataFrame
        for doc in doc_series:
            loc_doc = []

            for sent_index, sentence in enumerate(doc.sentences):
                lemmas = [word.lemma for word in sentence.words]
                for vocab in vocab_row:
                    location = first_occurrence_of_term_in_sent(vocab, lemmas)
                    if location:
                        loc_doc.append((sent_index, location))  # namedtuple?
            loc_docs.append(loc_doc)
        loc_phrases.append(loc_docs)
    column_mid_name = " " + CONTEXT + " " if is_context else " "
    column_full_name = column + column_mid_name + LOCATIONS
    return column_full_name, loc_phrases


def print_words_at_locations(vocabulary, texts, is_context=False):
    column_mid_name = " " + CONTEXT + " " if is_context else " "
    location_series = vocabulary[READER + column_mid_name + LOCATIONS]
    lemma_series = texts[COL_LEMMA]
    for location_row in location_series:
        for doc_index, loc_doc in enumerate(location_row):
            for loc_sent in loc_doc:
                sent_index = loc_sent[0]
                vocab = loc_sent[1]
                lemma = lemma_series[doc_index][sent_index][slice(*vocab)]
                print(lemma)


def collect_all_vocab_contexts_in_texts(
    vocabs: DataFrame, texts: DataFrame, column: str
) -> (str, Series):
    name = column + " " + CONTEXT
    series = vocabs.apply(
        lambda x: collect_vocab_context_in_texts(
            x[column + " " + LOCATIONS], texts[COL_STANZA_DOC]
        ),
        axis=1,
    )
    return name, series


def collect_vocab_context_in_texts(
    vocab_locations: [[(int, (int, int))]], text_docs: Series, window: int = 3
) -> [str]:
    context = []

    for doc_index, doc_loc in enumerate(vocab_locations):
        for sent_loc in doc_loc:
            start, end = sent_loc[1]
            sent = text_docs[doc_index].sentences[sent_loc[0]]

            from string import punctuation

            sent_lemmas = [
                word.lemma.lower()
                for word in sent.words
                if word.lemma not in punctuation + r"¡¿–—"
            ]

            # optimized string replacement (implementation in C)
            # works on strings, not arrays, thus we don't use it for the moment
            # some_str.translate(str.maketrans('', '', punctuation))

            # limit indices to sentence bounds using `min` and `max`
            # to safely use with list slicing
            # since out-of-bounds slicing would return empty list ([])
            slice_before = slice(max(0, start - window), start)
            slice_after = slice(end, min(end + window, len(sent_lemmas)))

            context_words = sent_lemmas[slice_before] + sent_lemmas[slice_after]
            context.extend(context_words)

    # Remove duplicates and return
    return list(set(context))


# =========================== PIPELINE EXECUTION ==============================
# Pipeline steps receive a DataFrame, transform it, and return it for next step

shrink_content_step = (shrink_text_content, COL_RAW_TEXT, COL_RAW_TEXT)
text_analysis_pipeline_simple = [
    (read_files, COL_TEXT_FILE, COL_RAW_TEXT),
    (make_stanza_docs_simple, COL_RAW_TEXT, COL_STANZA_DOC),
    (get_fields, COL_STANZA_DOC, COL_LEMMA, ("lemma",)),
    (normalize, COL_LEMMA, COL_LEMMA),
]
text_analysis_pipeline_ner = [
    (read_files, COL_TEXT_FILE, COL_RAW_TEXT),
    (make_stanza_docs_ner, COL_RAW_TEXT, COL_STANZA_DOC),
    (get_fields, COL_STANZA_DOC, COL_LEMMA, ("lemma",)),
    (normalize, COL_LEMMA, COL_LEMMA),
    (remove_stopwords, COL_LEMMA, COL_LEMMA),
]
text_analysis_pipeline = [
    (read_files, COL_TEXT_FILE, COL_RAW_TEXT),
    (make_stanza_docs, COL_RAW_TEXT, COL_STANZA_DOC),
    (get_fields, COL_STANZA_DOC, COL_LEMMA, ("lemma",)),
    (get_fields, COL_STANZA_DOC, "Text", ("text",)),
    (normalize, COL_LEMMA, COL_LEMMA),
]
vocabulary_pipeline = [
    (make_stanza_docs, COL_LEXICAL_ITEM, COL_STANZA_DOC),
    (get_fields, COL_STANZA_DOC, COL_LEMMA, ("lemma",)),
    (normalize, COL_LEMMA, COL_LEMMA),
]


def update_dataframe_with_func(df, func, src, dst, args=()):
    if dst not in df.columns or src == dst:
        df[dst] = func(df[src], *args)
    return df


def run(df: DataFrame, pipes: [Pipe]) -> DataFrame:
    for params in pipes:
        df = update_dataframe_with_func(df, *params)
    return df


def post_process_lemmatization(df: DataFrame) -> DataFrame:
    """
    Some lemmas are not correctly lemmatized, thus we apply a manual fix.
    For these, we copy the original lexical item as it is already
    in the correct form and no further processing is needed.
    :return: None
    """

    src = "Lexical item"
    dst = "Lemma"
    items_to_fix = lexical_items_to_skip_lemmatization

    def fix_items(row):
        if row[src] in items_to_fix:
            row[dst] = [[row[src]]]
        return row

    return df.apply(fix_items, axis="columns")


lexical_items_to_skip_lemmatization = []
