stats
=========

command line statistics

Feed in statistics on standard input and see number of samples, average, minimum, 1st quartile, median, 3rd quartile and maximum. Input can be prefixed with a common string, only the last word needs to be a number value. Statistics will be computed for each run of lines with equal prefixes.

Some statistics about the byte length of the first 10,000 words in the dictionary:

```
> head -n 10000 /usr/share/dict/words | while read line; do echo $line | wc -c; done | stats
Samples: 10000.0
Average: 8.63739999999999
Minimum: 1.0
25th percentile: 7.0
50th percentile: 9.0
75th percentile: 10.0
90th percentile: 12.0
99th percentile: 14.0
99.9th percentile: 17.0
Maximum: 23.0
```

Some statistics about the number of $2 bills printed per year from 1980-2012:

```
> curl --silent "https://explore.data.gov/resource/annual-production-figures-of-united-states-currency.csv" \
  | perl -pe 's/(FY \d{4})/"$1"/g;s|N/A|"0"|g' \
  | awk -F'","|^"|"$' '{sub("^\"","")} {print $3}' \
  | grep -v '^$' \
  | perl -pe 's/,//g' \
  | stats
Samples: 33.0
Average: 3.4133333333333336e7
Minimum: 0.0
25th percentile: 0.0
50th percentile: 0.0
75th percentile: 0.0
90th percentile: 1.1776000000000001e8
99th percentile: 4.0447999999999994e8
99.9th percentile: 4.78208e8
Maximum: 4.864e8
```

[![Build Status](https://secure.travis-ci.org/cheecheeo/stats.png)](http://travis-ci.org/cheecheeo/stats)
