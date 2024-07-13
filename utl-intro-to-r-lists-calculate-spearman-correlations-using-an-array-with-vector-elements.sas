%let pgm=utl-intro-to-r-lists-calculate-spearman-correlations-using-an-array-with-vector-elements;

Introduction to r lists calculate spearman correlations using an array with vector elements;


Problem

 Compute the four spearman correlations between corresponding array elements

  Sections

     1. sas formulation
     2. r elegant?
        https://stackoverflow.com/users/26315644/other

github
https://tinyurl.com/4umt9y2h
https://github.com/rogerjdeangelis/utl-intro-to-r-lists-calculate-spearman-correlations-using-an-array-with-vector-elements

stackoverflow
https://tinyurl.com/5n7433mf
https://stackoverflow.com/questions/78738920/is-there-a-way-to-calculate-a-spearmans-correlation-row-by-row-in-a-data-frame


/*                __                            _       _   _
 ___  __ _ ___   / _| ___  _ __ _ __ ___  _   _| | __ _| |_(_) ___  _ __
/ __|/ _` / __| | |_ / _ \| `__| `_ ` _ \| | | | |/ _` | __| |/ _ \| `_ \
\__ \ (_| \__ \ |  _| (_) | |  | | | | | | |_| | | (_| | |_| | (_) | | | |
|___/\__,_|___/ |_|  \___/|_|  |_| |_| |_|\__,_|_|\__,_|\__|_|\___/|_| |_|

*/

/*----                                                                   ----*/
/*----  In r we can have lists within lists. Suppose we want             ----*/
/*----  to compute the correlation of corresponding x y vectors           ----*/
/*----  Below is a possible SAS list within lists                        ----*/
/*----  Unlike r sas cannot operate directly list within lists            ---*/
/*----  This is one of the strengths in R.                               ----*/
/*----                                                                   ----*/
/*----                                                                   ----*/
    * r list structure in SAS;
    * think of each array element as a vector;
    array xs $32 x1-x4 (
          '1,2,3,4,5,6,7'
         ,'2,3,4,5,6,7,1'
         ,'3,4,5,6,7,1,2'
         ,'4,5,6,7,1,2,3');

    array ys $32 y1-y4 (
          '1,2,3,4,5,6,7'
         ,'2,3,4,5,6,7,1'
         ,'3,4,5,6,7,1,2'
         ,'4,5,6,7,1,2,3');

/*----                                                                   ----*/
/*----  the correlation of xs[1] with ys[1] can be done this way         ----*/
/*----  clumsy in sas                                                    ----*/
/*----                                                                   ----*/
/*----  data xy;                                                         ----*/
/*----  input x y; * xs[1] ys[1];                                        ----*/
/*----  cards4;                                                          ----*/
/*----  1 5                                                              ----*/
/*----  2 3                                                              ----*/
/*----  3 7                                                              ----*/
/*----  4 9                                                              ----*/
/*----  5 2                                                              ----*/
/*----  6 3                                                              ----*/
/*----  7 9                                                              ----*/
/*----  ;;;;                                                             ----*/
/*----  run;quit;                                                        ----*/
/*----                                                                   ----*/
/*----  proc corr data=xy spearman;                                      ----*/
/*----  run;quit;                                                        ----*/
/*----                                                                   ----*/
/*----  rho = 0.12729                                                    ----*/
/*----                                                                   ----*/

/*___                _                        _
|___ \   _ __    ___| | ___  __ _  __ _ _ __ | |_
  __) | | `__|  / _ \ |/ _ \/ _` |/ _` | `_ \| __|
 / __/  | |    |  __/ |  __/ (_| | (_| | | | | |_
|_____| |_|     \___|_|\___|\__, |\__,_|_| |_|\__|
                            |___/
*/

libname sd1 "d:/sd1";
proc datasets lib=sd1 nodetails nolist;
 delete want;
run;quit;

%utl_rbeginx;
parmcards4;
source("c:/oto/fn_tosas9x.R")
X <- list(c(1, 2, 3, 4, 5, 6, 7), c(2, 3, 4, 5, 6, 7, 1), c(3, 4, 5, 6, 7, 1, 2), c(4, 5, 6, 7, 1, 2, 3))
Y <- list(c(5, 3, 7, 9, 2, 3, 9), c(6, 7, 9, 2, 3, 9, 5),c(7, 9, 2, 3, 4, 5, 9), c(1, 9, 3, 4, 5, 6, 9))
str(X)
str(Y)
all_comb <- data.frame(X = I(X), Y = I(Y))
str(all_comb)
all_comb
b = apply(all_comb, 1, function(x) {
  cor.test(x[[1]], x[[2]], method = "spearman")
})
b
str(b)
cr<-data.frame(row=double(),cor=double())
for (i in seq(1,4,1)) {
 cr[i,1]<-i
 cr[i,2]<-b[[i]][[4]]
 }
cr
b[[1]][[4]]
b[[2]][[4]]
b[[3]][[4]]
b[[4]][[4]]
fn_tosas9x(
      inp    = cr
     ,outlib ="d:/sd1/"
     ,outdsn ="want"
     )
;;;;
%utl_rendx;

proc print data=sd1.want;
run;quit;

/**************************************************************************************************************************/
/*                                                                                                                        */
/*   ROW       COR                                                                                                        */
/*                                                                                                                        */
/*    1      0.12729                                                                                                      */
/*    2      0.09009                                                                                                      */
/*    3     -0.57660                                                                                                      */
/*    4     -0.32434                                                                                                      */
/*                                                                                                                        */
/*------------------------------------------------------------------------------------------------------------------------*/
/*                                                                                                                        */
/*  EXPLANATION                                                                                                           */
/*                                                                                                                        */
/*  REMEMBER FROM SAS                                                                                                     */
/*  -----------------                                                                                                     */
/*                                                                                                                        */
/*  array xs $32 x1-x4 (                                                                                                  */
/*        '1,2,3,4,5,6,7'                                                                                                 */
/*       ,'2,3,4,5,6,7,1'                                                                                                 */
/*       ,'3,4,5,6,7,1,2'                                                                                                 */
/*       ,'4,5,6,7,1,2,3');                                                                                               */
/*                                                                                                                        */
/*  array ys $32 y1-y4 (                                                                                                  */
/*        '1,2,3,4,5,6,7'                                                                                                 */
/*       ,'2,3,4,5,6,7,1'                                                                                                 */
/*       ,'3,4,5,6,7,1,2'                                                                                                 */
/*       ,'4,5,6,7,1,2,3');                                                                                               */
/*                                                                                                                        */
/* HERE IS THE R EQUIVALENT                                                                                               */
/* ------------------------                                                                                               */
/*                                                                                                                        */
/* $ X:List of 4                                                                                                          */
/*  ..$ : num  1 2 3 4 5 6 7                                                                                              */
/*  ..$ : num  2 3 4 5 6 7 1                                                                                              */
/*  ..$ : num  3 4 5 6 7 1 2                                                                                              */
/*  ..$ : num  4 5 6 7 1 2 3                                                                                              */
/* $ Y:List of 4                                                                                                          */
/*  ..$ : num  5 3 7 9 2 3 9                                                                                              */
/*  ..$ : num  6 7 9 2 3 9 5                                                                                              */
/*  ..$ : num  7 9 2 3 4 5 9                                                                                              */
/*  ..$ : num  1 9 3 4 5 6 9                                                                                              */
/*                                                                                                                        */
/* R can operate on this directly                                                                                         */
/*                                                                                                                        */
/* b = apply(list, 1, function(x) {                                                                                       */
/*   cort(x[[1]], x[[2]], method = "spearman")                                                                            */
/* })                                                                                                                     */
/*                                                                                                                        */
/*                                                                                                                        */
/* OUTPUT (LIKE A ODS AND A PROC)                                                                                         */
/*                                                                                                                        */
/* The fourh element in the first list BELOW is  b[[1]][[4]]                                                              */
/*                                                                                                                        */
/* If you want to list the four correlation coeficients you could                                                         */
/* just put these lines in you r script                                                                                   */
/*                                                                                                                        */
/* Elements of the b list                                                                                                 */
/*                                                                                                                        */
/* b[[1]][[4]]  correlation xs[1] with ys[1] in SAS                                                                       */
/* b[[2]][[4]]                                                                                                            */
/* b[[3]][[4]]                                                                                                            */
/* b[[4]][[4]]                                                                                                            */
/*                                                                                                                        */
/* > b[[1]][[4]]                                                                                                          */
/*       rho                                                                                                              */
/* 0.1272938                                                                                                              */
/* > b[[2]][[4]]                                                                                                          */
/*        rho                                                                                                             */
/* 0.09009375                                                                                                             */
/* > b[[3]][[4]]                                                                                                          */
/*     rho                                                                                                                */
/* -0.5766                                                                                                                */
/* > b[[4]][[4]]                                                                                                          */
/*        rho                                                                                                             */
/* -0.3243375                                                                                                             */
/*                                                                                                                        */
/* Like a proc there are other statistics available                                                                       */
/*   _                   _               _     _ _ _     _                                                                */
/*  | |__     ___  _   _| |_ _ __  _   _| |_  | (_|_)___| |_                                                              */
/*  | `_ \   / _ \| | | | __| `_ \| | | | __| | | | / __| __|                                                             */
/*  | |_) | | (_) | |_| | |_| |_) | |_| | |_  | | | \__ \ |_                                                              */
/*  |_.__/   \___/ \__,_|\__| .__/ \__,_|\__| |_|_|_|___/\__|                                                             */
/*                          |_|                                                                                           */
/*  > str(b)                                                                                                              */
/*  List of 4                                                                                                             */
/*   $ :List of 8                                                                                                         */
/*    ..$ statistic  : Named num 48.9                                                                                     */
/*    .. ..- attr(*, "names")= chr "S"                                                                                    */
/*    ..$ parameter  : NULL                        Index for rho                                                          */
/*    ..$ p.value    : num 0.786                                                                                          */
/*    ..$ estimate   : Named num 0.127  ---------  b[[1]][[4]] ----------------  data xy;                                 */
/*                                                                               input x y; * xs[1] ys[1];                */
/*                                                                               cards4;                                  */
/*                                                                               1 5                                      */
/*                                                                               2 3                                      */
/*    .. ..- attr(*, "names")= chr "rho"                                         3 7                                      */
/*    ..$ null.value : Named num 0                                               4 9                                      */
/*    .. ..- attr(*, "names")= chr "rho"                                         5 2                                      */
/*    ..$ alternative: chr "two.sided"                                           6 3                                      */
/*    ..$ method     : chr "Spearman's rank correlation rho"                     7 9                                      */
/*    ..$ data.name  : chr "x[[1]] and x[[2]]"                                   ;;;;                                     */
/*    ..- attr(*, "class")= chr "htest"                                          run;quit;                                */
/*   $ :List of 8                                                                                                         */
/*    ..$ statistic  : Named num 51                                              proc corr data=xy spearman;              */
/*    .. ..- attr(*, "names")= chr "S"                                           run;quit;                                */
/*    ..$ parameter  : NULL                                                                                               */
/*    ..$ p.value    : num 0.848                                                 rho = 0.12729                            */
/*    ..$ estimate   : Named num 0.0901                                                                                   */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ null.value : Named num 0                                                                                        */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ alternative: chr "two.sided"                                                                                    */
/*    ..$ method     : chr "Spearman's rank correlation rho"                                                              */
/*    ..$ data.name  : chr "x[[1]] and x[[2]]"                                                                            */
/*    ..- attr(*, "class")= chr "htest"                                                                                   */
/*   $ :List of 8                                                                                                         */
/*    ..$ statistic  : Named num 88.3                                                                                     */
/*    .. ..- attr(*, "names")= chr "S"                                                                                    */
/*    ..$ parameter  : NULL                                                                                               */
/*    ..$ p.value    : num 0.175                                                                                          */
/*    ..$ estimate   : Named num -0.577                                                                                   */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ null.value : Named num 0                                                                                        */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ alternative: chr "two.sided"                                                                                    */
/*    ..$ method     : chr "Spearman's rank correlation rho"                                                              */
/*    ..$ data.name  : chr "x[[1]] and x[[2]]"                                                                            */
/*    ..- attr(*, "class")= chr "htest"                                                                                   */
/*   $ :List of 8                                                                                                         */
/*    ..$ statistic  : Named num 74.2                                                                                     */
/*    .. ..- attr(*, "names")= chr "S"                                                                                    */
/*    ..$ parameter  : NULL                                                                                               */
/*    ..$ p.value    : num 0.478                                                                                          */
/*    ..$ estimate   : Named num -0.324                                                                                   */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ null.value : Named num 0                                                                                        */
/*    .. ..- attr(*, "names")= chr "rho"                                                                                  */
/*    ..$ alternative: chr "two.sided"                                                                                    */
/*    ..$ method     : chr "Spearman's rank correlation rho"                                                              */
/*    ..$ data.name  : chr "x[[1]] and x[[2]]"                                                                            */
/*    ..- attr(*, "class")= chr "htest"                                                                                   */
/*                                                                                                                        */
/*                                                                                                                        */
/* If you put this single letter in your R script ypu get                                                                 */
/*                                                                                                                        */
/* b                                                                                                                      */
/*                                                                                                                        */
/*  Spearman's rank correlation rho                                                                                       */
/*                                                                                                                        */
/*  data:  x[[1]] and x[[2]]                                                                                              */
/*  S = 48.872, p-value = 0.7856                                                                                          */
/*  alternative hypothesis: true rho is not equal to 0                                                                    */
/*  sample estimates:                                                                                                     */
/*        rho                                                                                                             */
/*  0.1272938                                                                                                             */
/*                                                                                                                        */
/*                                                                                                                        */
/*  [[2]]                                                                                                                 */
/*                                                                                                                        */
/*  Spearman's rank correlation rho                                                                                       */
/*                                                                                                                        */
/*  data:  x[[1]] and x[[2]]                                                                                              */
/*  S = 50.955, p-value = 0.8477                                                                                          */
/*  alternative hypothesis: true rho is not equal to 0                                                                    */
/*  sample estimates:                                                                                                     */
/*         rho                                                                                                            */
/*  0.09009375                                                                                                            */
/*                                                                                                                        */
/*                                                                                                                        */
/*  [[3]]                                                                                                                 */
/*                                                                                                                        */
/*  Spearman's rank correlation rho                                                                                       */
/*                                                                                                                        */
/*  data:  x[[1]] and x[[2]]                                                                                              */
/*  S = 88.29, p-value = 0.1754                                                                                           */
/*  alternative hypothesis: true rho is not equal to 0                                                                    */
/*  sample estimates:                                                                                                     */
/*      rho                                                                                                               */
/*  -0.5766                                                                                                               */
/*                                                                                                                        */
/*                                                                                                                        */
/*  [[4]]                                                                                                                 */
/*                                                                                                                        */
/*  Spearman's rank correlation rho                                                                                       */
/*                                                                                                                        */
/*  data:  x[[1]] and x[[2]]                                                                                              */
/*  S = 74.163, p-value = 0.4779                                                                                          */
/*  alternative hypothesis: true rho is not equal to 0                                                                    */
/*  sample estimates:                                                                                                     */
/*         rho                                                                                                            */
/*  -0.3243375                                                                                                            */
/*                                                                                                                        */
/**************************************************************************************************************************/

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
