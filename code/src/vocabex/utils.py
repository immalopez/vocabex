from typing import Optional, Tuple
import time


class NoPool:
    def __init__(self):
        pass

    def __enter__(self):
        return self

    def __exit__(self, *args):
        pass

    def map(self, func, iterable):
        return list(map(func, iterable))


def calculatestar(args):
    return calculate(*args)


def calculate(func, args):
    return func(*args)


# TODO: Optimize search to look for sub-lists only when needed
def first_occurrence_of_term_in_sent(
        term: [str],
        sentence: [str]
) -> Optional[Tuple[int, int]]:
    """Returns a tuple(start, end) where start is the first index of
    vocabulary in text and end is the end index of the vocabulary in text if
    the lexical items contained in a vocabulary list are to be found in the
    sentence lists of a given text, and None otherwise."""

    term_num_words = len(term)  # is it a multi-word term?
    if term_num_words == 1:
        try:
            index = sentence.index(term[0])
            return index, index + 1
        except ValueError:
            return None

    term_index = 0
    sent_index = 0
    sent_len = len(sentence)
    while sent_index < sent_len and term_index < term_num_words:
        if sentence[sent_index] == term[term_index]:
            sent_index += 1
            term_index += 1
            if term_index == term_num_words:
                # adjust start to include vocab item(s)
                return sent_index - term_num_words, sent_index  # a tuple
        else:
            sent_index = sent_index - term_index + 1
            term_index = 0
    return None


def duration(start, msg):
    total = time.time() - start
    if total < 60:
        print(f'{msg} ({total:.2f}s)')
    else:
        print(f'{msg} ({total/60:.0f}m {total%60:.0f}s)')
    print('---')
