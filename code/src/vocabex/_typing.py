##############################################################################
#                               TYPE ANNOTATIONS                             #
##############################################################################

import pandas as pd
from collections.abc import Callable


DataFrameGroupBy = pd.core.groupby.generic.DataFrameGroupBy
DataFrame = pd.DataFrame
Series = pd.Series
Pipe = Callable[[DataFrame], DataFrame]

