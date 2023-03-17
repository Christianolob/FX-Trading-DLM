from forex_python.converter import CurrencyRates
import pandas as pd
import datetime
import os
import time


# Initialize the currency converter
c = CurrencyRates()

# Define the start and end date for the data
start_date = '2000-01-01'
# end_date = '2022-12-31'
end_date = '2023-12-31'

# Define the base and target currencies
base_currency = 'EUR'
target_currency = 'USD'

g10 = ["NZD"]
# g10 = ["CAD","NOK","SEK","JPY"]

script_dir = os.path.dirname(os.path.abspath(__file__))

# Initialize an empty list to store the exchange rate data
exchange_rates = []

# hoje = datetime.datetime.today()
# start = datetime.datetime.strptime(str(hoje), '%Y-%m-%d %H:%M:%S')

start = datetime.datetime.strptime(str(min(pd.date_range(start_date, end_date))), '%Y-%m-%d %H:%M:%S')
end = datetime.datetime.strptime(str(max(pd.date_range(start_date, end_date))), '%Y-%m-%d %H:%M:%S')

start = start.year
end = end.year
this_year = end

#ano_atual  = end.year

strongs_vs_usd = ["EUR","GBP","AUD","NZD"]
weak_vs_usd = ["CAD","NOK","SEK","JPY"]

for currency in g10:
    if currency == "USD":
        pass
    print(currency)
    for year in range(start,end):
        print(year)
        # Initialize an empty list to store the exchange rate data
        exchange_rates = []
        for date in pd.date_range(str(year)+"-01-01", str(year)+"-12-31"):
            print(str(date))
            if date.dayofweek > 4:
                continue
            date = datetime.datetime.strptime(str(date), '%Y-%m-%d %H:%M:%S')
            if currency in strongs_vs_usd:
                base = currency
                weak = "USD"
            if currency in weak_vs_usd:
                base = "USD"
                weak = currency
            rate = c.get_rate(base, weak, date)
            time.sleep(1)
            exchange_rates.append((date, rate))
        # Convert the exchange rate data to a pandas DataFrame
        df = pd.DataFrame(exchange_rates, columns=['date', 'exchange_rate'])
        df.set_index('date', inplace=True)
        # Define the file name
        file_name = f'{currency}_exchange_rate_{year}.csv'
        # Define the file path
        file_path = os.path.join(script_dir, file_name)
        # Save the DataFrame to a CSV file
        df.to_csv(file_path, index=True)


