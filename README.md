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

[![Build Status](https://secure.travis-ci.org/cheecheeo/stats.png)](http://travis-ci.org/cheecheeo/stats)
