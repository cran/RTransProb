# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

getTransitionProbability <- function(sampleTotals_totalsMat, sampleTotals_totalsVec, sampleTotals_methods, snapshots, interval) {
    .Call(`_RTransProb_getTransitionProbability`, sampleTotals_totalsMat, sampleTotals_totalsVec, sampleTotals_methods, snapshots, interval)
}

getTransitionProbabilityTNH <- function(sampleTotals_totalsMat, snapshots, interval) {
    .Call(`_RTransProb_getTransitionProbabilityTNH`, sampleTotals_totalsMat, snapshots, interval)
}

getCategoriesLocal <- function(ind, numDate_, numericalRating_, StartPos_, sDate, eDate) {
    .Call(`_RTransProb_getCategoriesLocal`, ind, numDate_, numericalRating_, StartPos_, sDate, eDate)
}

getRatingsSnapshotsLocal <- function(ind, numDate_, numericalRating_, StartPos_, snapshotDates_) {
    .Call(`_RTransProb_getRatingsSnapshotsLocal`, ind, numDate_, numericalRating_, StartPos_, snapshotDates_)
}

getidTotCntCohortRCPP <- function(nIDs, numDate, numericalRating, StartPos, snapshotDates, nRtgsCatNaN) {
    .Call(`_RTransProb_getidTotCntCohortRCPP`, nIDs, numDate, numericalRating, StartPos, snapshotDates, nRtgsCatNaN)
}

getidTotCntDurationRCPP <- function(nIDs, numDate, numericalRating, StartPos, nRtgsCatNaN, algo, sDate, eDate) {
    .Call(`_RTransProb_getidTotCntDurationRCPP`, nIDs, numDate, numericalRating, StartPos, nRtgsCatNaN, algo, sDate, eDate)
}

getSampleTotals <- function(lsttotals_totalsMat, lsttotals_totalsVec, totals_methods, sliceCnt, rownum, colnum) {
    .Call(`_RTransProb_getSampleTotals`, lsttotals_totalsMat, lsttotals_totalsVec, totals_methods, sliceCnt, rownum, colnum)
}

getExpandTransData <- function(industryName, Qtr_Year, endDate, Rating_Trans, start_Rating, end_rating, mCount, wgtname, transData_nm_cnt, totalCountExpanded) {
    .Call(`_RTransProb_getExpandTransData`, industryName, Qtr_Year, endDate, Rating_Trans, start_Rating, end_rating, mCount, wgtname, transData_nm_cnt, totalCountExpanded)
}

