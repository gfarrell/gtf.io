from matplotlib import pyplot

# data source: https://www.iab.com/insights/iab-internet-advertising-revenue-report-conducted-by-pricewaterhousecoopers-pwc-2/

output_file = "../images/iab-revenues.png"

data = [
 ("2000", 8.23),
 ("2001", 7.21),
 ("2002", 6.01),
 ("2003", 7.27),
 ("2004", 9.63),
 ("2005", 12.54),
 ("2006", 16.88),
 ("2007", 21.21),
 ("2008", 23.45),
 ("2009", 22.66),
 ("2010", 26.04),
 ("2011", 31.74),
 ("2012", 36.57),
 ("2013", 42.78),
 ("2014", 49.45),
 ("2015", 59.60),
 ("2016", 72.50),
 ("2017", 88.00),
]

x_vals = [datum[0] for datum in data]
y_vals = [datum[1] for datum in data]

pyplot.bar(x_vals, y_vals)
pyplot.title("Online advertising revenues")
pyplot.xlabel("Year")
pyplot.ylabel("Revenue (USD billions)")
pyplot.xticks(rotation=90)
pyplot.savefig(output_file, pad_inches=0.1, bbox_inches="tight")
