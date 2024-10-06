import numpy as np
import pandas as pd
import random as rd

# Initialize our example dataframe with 6 columns: sku, retailer, promotion type,
# promotion id, date, selling
df = pd.DataFrame(
    {
        'sku': [1] * 25,
        'retailer': ['A'] * 20 + ['B'] * 5,
        'promotion_type': [1] * 10 + [2] * 10 + [2] * 3 + [3] * 2,
        'promotion_id': ['A1-2019'] * 2 + ['A1-20201'] * 8 + ['A2-20201'] * 3 + \
                        ['A2-20203'] * 5 + ['A2-20204'] * 2 + ['B2-20201'] * 3 + \
                        ['B3-20201'] * 2,
        'date': [
            pd.Timestamp(el) for el in ["2019-12-01", "2019-12-02", "2020-01-01",
                                        "2020-01-02", "2020-01-03", "2020-01-04",
                                        "2020-01-05", "2020-01-06", "2020-01-07",
                                        "2020-01-08", "2020-01-21", "2020-01-22",
                                        "2020-01-23", "2020-03-01", "2020-03-02",
                                        "2020-03-03", "2020-03-04", "2020-03-05",
                                        "2020-04-15", "2020-04-16", "2020-01-21",
                                        "2020-01-22", "2020-01-23", "2020-01-21",
                                        "2020-01-22"]],
        'sellin': [rd.randint(100, 200) for i in range(25)]
    }
)
# rest of your code...
