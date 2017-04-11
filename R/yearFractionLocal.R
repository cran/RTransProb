yearFractionLocal <- function(date1,date2){

  mdate2 <- as.Date(date2-1, origin = '0000-01-01')
  year2 = as.numeric(format(mdate2,'%Y'))

  mdate1 <- as.Date(date1-1, origin = '0000-01-01')
  year1 = as.numeric(format(mdate1,'%Y'))
  month1 = as.numeric(format(mdate1,'%m'))
  day1  = as.numeric(format(mdate1,'%d'))

  dayDiffYr2 = date2-POSIXTomatlab(as.POSIXlt(as.Date(ISOdate(year2,month1,day1),format = "%m/%d/%Y")))


  ly2 = ((year2%%4 == 0 & year2%%100 != 0) | year2%%400 == 0);
  daysInYr2 = 365 + ly2;
  frac = year2-year1+dayDiffYr2/daysInYr2;

  return(frac)
}
