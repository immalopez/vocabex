import os
from enum import Enum
from collections import namedtuple
from typing import Optional

import pandas as pd

pd.set_option('display.max_columns', 99)

FOLDER_DATA = './data/'
FOLDER_DATA_TRIAL = './data_trial/'


# ================================= LOAD DATA =================================
is_trial = False


def read_pandas_csv(path: str, sep=";") -> pd.DataFrame:
    # print(f'\tReading csv file at {path}')
    return pd.read_csv(path, sep=sep)


# ================================= LOAD DATA =================================


def read_files(paths):
    """Opens each text file in a list and returns a single list of strings with
    all of their contents."""
    texts = []
    for path in paths:
        with open(path) as file:
            text = file.read()
            texts.append(text)
    return texts
