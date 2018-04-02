test_that('Throws errors', {
  throws_error(fars_read_years(years = 2013))
  throws_error(fars_summarize_years(years = 2013))
  throws_error(make_filename(year = 'two thousand thirteen'))
})
