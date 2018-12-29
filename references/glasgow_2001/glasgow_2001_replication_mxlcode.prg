@ Original code by Kenneth Train, David Revelt, and Paul Ruud @
@ Copyright (c) 1996, 1999 Kenneth Train, David Revelt, and Paul Ruud @
@ The original version of this code is available on Kenneth Train's website @
@ at http://www.elsa.berkeley.edu/~train/                                   @
@ README files and test datasets are available there.   @

@ This code replicates Table 2 in Glasgow, G. (2001) "Mixed Logit Models @ 
@ for Multiparty Elections", Political Analysis 9(2) @
@ It uses the dataset glasgow.data.asc @
@ Note that this code can easily be modified to replicate Table 1, @
@ compute predicted vote shares, or estimate vaote shares for a @
@ hypothetical voter (as in Table 3 and Figure 1) @


@ A copy of this code is also available on my homepage @
@ at http://www.polsci.ucsb.edu/faculty/glasgow @
@ I welcome questions or comments @
@ I can be reached at glasgow@sscf.ucsb.edu  @

@ Modified from the original by Garrett Glasgow, 3/9/00 @
@ Now has an option to calculate standard errors on @
@ forecasted shares through multiple draws from the multivariate @
@ normal distribution of the coefficients. @

@ Memory space allocation: increase as needed by input data @
new , 30000;


@ Output to screen? (in addition to file) @
screen on;

@ Name of the output file, reset=overwrite existing file, on=add to it @
output file=glasgow.out reset;

@ Put a title for your output in the quotes below. @
print " ";
print ;

@ 1 to check inputs for conformability and consistency, else 0. @
VERBOSE = 1;

@ Number of observations @
NOBS = 2131;

@ Maximum number of alternatives represented in any choice situation@
NALT = 3;

@ Create or load XMAT @
@ XMAT here contains all explanatory variables and the dependent variable @

@ Create your data matrix XMAT here.  It must contain all of the @
@ necessary variables, including the dependent variable and censoring @
@ variable (if needed.) @

@----------------------------------------------------------------@

@ READING IN DATASET @

load XMAT1[NOBS,39] = glasgow.data.asc;

@ Censoring variable @
CC = ones(NOBS,2)~zeros(NOBS,1);

@ Define the variables @

id        = XMAT1[.,1];
votechc   = XMAT1[.,2];
respinf   = XMAT1[.,3];
respunem  = XMAT1[.,4];
resptax   = XMAT1[.,5];
south     = XMAT1[.,6];
mid       = XMAT1[.,7];
north     = XMAT1[.,8];
wales     = XMAT1[.,9];
scot      = XMAT1[.,10];
union2    = XMAT1[.,11];
public    = XMAT1[.,12];
blue      = XMAT1[.,13];
women     = XMAT1[.,14];
age       = XMAT1[.,15];
homeown   = XMAT1[.,16];
faminc    = XMAT1[.,17];
educage   = XMAT1[.,18];
disdef1   = XMAT1[.,19];
disphil1  = XMAT1[.,20];
distax1   = XMAT1[.,21];
disnat1   = XMAT1[.,22];
disredi1  = XMAT1[.,23];
discrim1  = XMAT1[.,24];
diswelf1  = XMAT1[.,25];
disdef2   = XMAT1[.,26];
disphil2  = XMAT1[.,27];
distax2   = XMAT1[.,28];
disnat2   = XMAT1[.,29];
disredi2  = XMAT1[.,30];
discrim2  = XMAT1[.,31];
diswelf2  = XMAT1[.,32];
disdef3   = XMAT1[.,33];
disphil3  = XMAT1[.,34];
distax3   = XMAT1[.,35];
disnat3   = XMAT1[.,36];
disredi3  = XMAT1[.,37];
discrim3  = XMAT1[.,38];
diswelf3  = XMAT1[.,39];

@ SETTING UP DATA MATRIX @

@ Alternative-specific variables (issues) @

XMAT = disdef1~disdef2~disdef3;
XMAT = XMAT~disphil1~disphil2~disphil3;
XMAT = XMAT~distax1~distax2~distax3;
XMAT = XMAT~disnat1~disnat2~disnat3;
XMAT = XMAT~disredi1~disredi2~disredi3;
XMAT = XMAT~discrim1~discrim2~discrim3;
XMAT = XMAT~diswelf1~diswelf2~diswelf3;

@ Individual-specific variables @

one = ones(NOBS,1);
z   = zeros(NOBS,1);

XMAT = XMAT~one~z~z;
XMAT = XMAT~z~one~z;
XMAT = XMAT~south~z~z;
XMAT = XMAT~z~south~z;
XMAT = XMAT~mid~z~z;
XMAT = XMAT~z~mid~z;
XMAT = XMAT~north~z~z;
XMAT = XMAT~z~north~z;
XMAT = XMAT~wales~z~z;
XMAT = XMAT~z~wales~z;
XMAT = XMAT~scot~z~z;
XMAT = XMAT~z~scot~z;
XMAT = XMAT~union2~z~z;
XMAT = XMAT~z~union2~z;
XMAT = XMAT~public~z~z;
XMAT = XMAT~z~public~z;
XMAT = XMAT~blue~z~z;
XMAT = XMAT~z~blue~z;
XMAT = XMAT~women~z~z;
XMAT = XMAT~z~women~z;
XMAT = XMAT~age~z~z;
XMAT = XMAT~z~age~z;
XMAT = XMAT~homeown~z~z;
XMAT = XMAT~z~homeown~z;
XMAT = XMAT~faminc~z~z;
XMAT = XMAT~z~faminc~z;
XMAT = XMAT~educage~z~z;
XMAT = XMAT~z~educage~z;
XMAT = XMAT~respinf~z~z;
XMAT = XMAT~z~respinf~z;
XMAT = XMAT~resptax~z~z;
XMAT = XMAT~z~resptax~z;
XMAT = XMAT~respunem~z~z;
XMAT = XMAT~z~respunem~z;

@ Dependent variable @
@ y1=Conservative y2=Labour y3=Alliance @
y1 = (votechc .==1);
y2 = (votechc .==2);
y3 = (votechc .==3);
XMAT = XMAT~y1~y2~y3;

XMAT = XMAT~CC;

@-----------------------------------------------------------------@

@ Number of variables in XMAT @
NVAR = 43;

@ Number of the variable in XMAT that is the dependent variable @
IDDEP = 42;

@ Number of explanatory variables that have fixed coefficients. @
NFC = 37;

@ NFCx1 vector to identify the variables in XMAT that have fixed @
@ coefficients. (column vector) @
IDFC = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
         22, 23, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37 };

@ Number of explanatory variables that have normally-distributed coefficients @
NNC = 0;

@ NNCx1 vector to identify the variables in XMAT that have normally @
@ distributed coefficients. (column vector) @
IDNC = { };

@ Number of explanatory variables that have uniformly-distributed coefficients @
NUC = 0;

@ NUCx1 vector to identify the variables in XMAT that have uniformly @
@ distributed coefficients. (column vector) @
IDUC = { };

@ Number of explanatory variables that have triangularly-distributed coefficients @
NTC = 4;

@ NTCx1 vector to identify the variables in XMAT that have triangularly @
@ distributed coefficients. (column vector) @
IDTC = { 20, 21, 24, 25 };

@ Number of explanatory variables that have log-normally distributed coeffs @
NLC = 0;

@ NLCx1 vector to identify the variables in XMAT that have @
@ log-normally distributed coefficients. (column vector) @
IDLC = { 0 };

@ 1 if all people do not face all NALT alternatives, else 0. @
CENSOR = 0;

@ If CENSOR: The number of the variable in XMAT which identifies which @
@ alternatives each person faces. @
IDCENSOR = { 43 }; 

@ If you want to weight the observation, set DOWGT to 1, else 0 @
DOWT = 0 ;

@ If DOWGT=1, load or specify a NOBSx1 vector of weights. (column vector) @
WEIGHT = { 0 };

@ 1 to print out the diagonal elements of the Hessian, else 0. @
PRTHESS = 0;

@ Rescaling XMAT...  @
@ 1 to rescale any variable in XMAT, else 0. @
RESCALE = 0;

@ If RESCALE = 1 create a q x 2 matrix to id which variables to @
@ rescale and by what multiplicative factor, else make RESCLMAT @
@ a dummy value. @
RSCLMAT = { 0 };

@ Starting values for the parameters. @
@ Modified by Garrett Glasgow, 3/9/00 @
@ When forecasting, instead of using just the means of the coefficients, @ 
@ here we provide the option of using means and the covariance matrix in @
@ order to draw from a multivariate normal multiple times (in order to get @
@ standard deviations on the forecasted shares) @

@ (NFC+(2*NNC)+(2*NUC)+(2*NTC)+(2*NLC)) x 1 vector of mean values. @
@ Order is NFC fixed coefficients, followed by mean and standard deviation of each @
@ of NNC normal coefficients, followed by mean and standard deviation of each of NUC@
@ uniformly distributed coefficients, etc for the triangularly and log-normally distributed coefficients.@ 

B = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 1, 0, 1, 0, 1 };

@ Covariance matrix of coefficients.  This should be saved from @
@ the original estimation.  @ 
@ It is a (NFC+(2*NNC)+(2*NUC)+(2*NTC)+(2*NLC)) x @
@ (NFC+(2*NNC)+(2*NUC)+(2*NTC)+(2*NLC)) matrix of covariances. @
@ The covariance matrix is saved as a .fmt file.  Change the filename @
@ after the = to the name of the saved matrix. @
@ Note this code could be easily modified to use the saved vector @
@ of coefficients as well.  @

@ Comment this out if it hasn't been created yet! @
@ If commented out, replace with "vc = 0" @
/* loadm vc = covmat1; */
vc = 0; 

@ 1 to constrain any parameter to its starting value, else 0. @
 CZVAR = 0;

@ If CZVAR = 1, create a vector of zeros and ones of the same @
@ dimension as B identifying which parameters are to vary, else make @
@ BACTIVE = { 0 }. (column vector) @
BACTIVE = { 0 };

@ 1 to constrain any of the error components to equal each other, else 0 @
CEVAR = 0;

@ If CEVAR=1, create a matrix of ones and zeros to constrain the @
@ necessary random parameters to be equal, else make EQMAT=0. (matrix)@
EQMAT = { 0 }; 

@ Number of repetitions used in the simulations.  NREP must be >= 1. @
NREP = 125;

@ Choose random or halton draws. DRAWS=1 for random, 2 for Halton.@
DRAWS=2;

@ If DRAWS=1, set seed to use in random number generator. SEED1 must be >= 0. (integer) @
SEED1 = 666;

@ If DRAWS=2, specify the following.@
@ HALT=1 to create Halton draws, HALT=2 to use previously created Halton draws. @
HALT=1;

@ If HALT=1, set SAVH=1 if you want to save the Halton draws that are created; 0 otherwise. @ 
SAVH=0;

@ HMNAME = path and name for file of halton sequences. Be sure to put double \\ where@
@ single \ appears in the path. @
@ If HALT=1 and SAVH=1, the Halton draws that are created are saved to this file.@
@ If HALT=2, the Haton draws that were previously saved to this file are loaded.@
@ Note that, if HALT=2, the sequences must meet the specs of the current model,@
@ that is, the same NREP, NOBS, number of random coefficients, and distribution for each coefficient.@

HMNAME="halton.out";

@ Maximum number of iterations in the maximization @
NITER = 500;

@ Tolerance for convergence. (small decimal) @
EPS = 1.e-4;

@ Forecasting Parameters....  @
@ 1 to forecast instead of estimate, else 0 @
FCAST = 0;

@ Modified by Garrett Glasgow, 3/9/00 @
@ Standard errors on forecasted shares?  Set =1 to use draws from a mean @
@ vector and covariance matrix, =0 to just use mean vector. @
SESHARES = 1;

@ Set number of draws from multivariate normal for forecasted shares.  @
NSIMS = 1000;

@ Optimization Parameters... @
@ Identify the optimization routine: 1 Paul Ruud's routine @
@                                    2 for Gauss's maxlik      @
OPTIM = 2; 

@ Specify the optimization algorithm/hessian calculation.(integer) @
@ With OPTIM=1, options are METHOD=1 for bhhh, 2 for nr.@
@ With OPTIM=2, options are METHOD=1 for steepest descent, 2 for bfgs, @
@                         3 for dfp, 4 for nr, 5 for bhhh, 6 for prcg.@
METHOD = 2;

@ 1 if OPTIM=1 and want to use modified iteration procedure, else 0. @
MNR = 0;

@ If OPTIM=1, set STEP to the step length the maximization routine @
@ should initially try. @
STEP = 1;

@ If OPTIM=1, then set ROBUST=1 if you want robust standard errors, or@
@ ROBUST=0 if you want regular standard errors. With OPTIM=2, this option doesn't exist.@
ROBUST=1;

@ Modified by Garrett Glasgow, 3/9/00 @
@ if SAVECOV = 1 the covariance matrix from estimation will be saved.  @
@ Note: This only works if OPTIM = 2 @
SAVECOV = 1;

@ Name of covariance matrix saved from estimation @
COVMAT = "covmat1";    

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@	You should not need to change anything below this line	     @@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ Create global for the number of estimated variables @
NEVAR = NFC+(NNC+NUC+NTC+NLC)*2;

@ Check inputs if VERBOSE=1 @
if ((VERBOSE /= 1) and (VERBOSE /= 0));
  print "VERBOSE must 0 or 1.";
  print "Program terminated.";
   stop;
endif;
if VERBOSE;
  pcheck;
else;
 print "The inputs have not been checked since VERBOSE=0.";
 print;
endif; 

@ Rescale the variables. @
if RESCALE;
  j = rows(RSCLMAT);
  i = 1;
  if VERBOSE;
    if (RSCLMAT[.,1] > NVAR);
      print "RSCLMAT identifies a variable that is not in the data set.";
      print "Program terminated.";
      stop;
    endif;
    print "Rescaling Data:";
    print "        Variable      Mult. Factor";
    do while i <= j;
      RSCLMAT[i,1] RSCLMAT[i,2];
      XMAT[.,(RSCLMAT[i,1]-1)*NALT+1:RSCLMAT[i,1]*NALT] = 
      XMAT[.,(RSCLMAT[i,1]-1)*NALT+1:RSCLMAT[i,1]*NALT] * RSCLMAT[i,2];
      i = i + 1;
    endo;
    print;
  else;
    do while i <= j;
      XMAT[.,(RSCLMAT[i,1]-1)*NALT+1:RSCLMAT[i,1]*NALT] =
      XMAT[.,(RSCLMAT[i,1]-1)*NALT+1:RSCLMAT[i,1]*NALT] * RSCLMAT[i,2];
      i = i + 1;
    endo;
  endif;
endif;

@ Print out starting values if VERBOSE = 1. @
if VERBOSE;
  print "The model has " NFC " fixed coefficients, for variables " IDFC;
  print "             " NNC "normally distributed coefficients, for variables " IDNC;
  print "             " NUC "uniformly distributed coefficients, for variables " IDUC;
  print "             " NTC "triangularly distributed coefficients, for variables " IDTC;
  print "          and " NLC "log-normally distributed coefficients, for variables " IDLC;
  print;
  if FCAST;
    print "Mean values used in forecasting are:";
    print B;
  endif;
  if CZVAR and (FCAST == 0);
    print "Starting values:" ;
    print B;
    print;
    print "Parameters that are estimated (1) or held at starting value (0):";
    print BACTIVE;
    print;
  endif;
  if (CZVAR == 0) and (FCAST == 0);
    print "Starting values:";
    print B;
    print;
    print "All parameters are estimated; none is held at its starting value.";
    print;
  endif;
endif;

@ Create new B and BACTIVE with reduced lengths if @
@ user specifies equality constraints. @
if CEVAR;
  if CZVAR;
   BACTIVE = EQMAT * BACTIVE;
   BACTIVE = BACTIVE .> 0;
  endif;
  B = (EQMAT * B) ./ (EQMAT * ones((NFC+2*NNC+2*NUC+2*NTC+2*NLC),1));
  if VERBOSE and CZVAR and (FCAST == 0);
   print;
   print "Some parameters are constrained to be the same.";
   print "Starting values of constrained parameters:";
   print B;
   print;
   print "Constrained parameters that are estimated (1) or held at starting value (0):";
   print BACTIVE;
   print;
  endif;
  if VERBOSE and (CZVAR == 0) and (FCAST == 0);
   print "Some parameters are constrained to be the same.";
   print "Starting values of constrained parameters:";
   print B;
   print;
   print "All constrained parameters are estimated; none is held at its starting value.";
   print;
  endif;
endif;

@ Describing random terms. @

if VERBOSE;
  if DRAWS == 1; 
    print "Random draws are used."; 
    print "Random error terms are based on:";
    print "Seed:  " SEED1;
    print;
  elseif DRAWS == 2;
    print "Halton draws are used.";
    print; 
  else;
    print "DRAWS must be 1 or 2. Program terminated.";
    stop;
  endif;
  print "Repetitions:  " NREP;
endif;

/* Number of random coefficients or 1, whichever is higher. */
NECOL = maxc( ones(1,1) | (NNC+NUC+NTC+NLC) );

if DRAWS == 2;
   /* CREATE OR LOAD HALTON SEQUENCES. */

   if HALT == 2; 
     loadm hm = ^HMNAME;
     print "Loaded Halton sequences.";
   elseif HALT == 1;

       @ Create the Halton sequence @

       print "Creating Halton sequences ....";


        /* Provide prime number vector */

       prim = { 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
                    73 79 83 89 97 101 103 107 109 113 };
       print "Halton sequences are based in primes: " prim[1,1:NECOL];
       print;

      /* Develop a halton matrix (hm) of size nrep x (nobs*necol); The first 'nobs'
      columns provide halton numbers for first dimension of integration,
      the second 'nobs' columns provide the halton numbers for the second
      dimension, and so on; cdfinvn is the inverse cumulative standard normal
      distribution function; PROC halton creates sequence; PROC cdfinvn takes
      inverse normal: windows version of gauss has this as a command. For
      non-normal distributions, use inverse of other distribution. */

      h = 1;
      hm = {};
      do while h <= (NECOL);
          hm1 = halton(10+nrep*nobs,prim[h]);
          if h <= NNC or h > (NNC+NUC+NTC);  @ Normal and lognormal distributions. @
                   @ If there are no random coefs, st NECOL=1 while NNC=NUC=NTC=NLC=0, @
                   @ then, hm1 will be treated as normally distributed.@
                hm1 = cdfinvn(hm1);
                @ The inverse-normal proc produces very extreme values sometimes. This truncates.@
                 hm1=hm1.*(hm1 .le 10) + 10 .* (hm1 .gt 10);
                 hm1=hm1.*(hm1 .ge -10) -10 .* (hm1 .lt -10);
          endif;
          if h > NNC and h <= (NNC+NUC);  @Uniform distribution.@
                 hm1=hm1.*2 - 1;  
          endif;
          if h > (NNC+NUC) and h <= (NNC+NUC+NTC); @Triangular distribution.@
                 hm1= (sqrt(2.*hm1)-1).*(hm1 .<= 0.5) + (1-sqrt(2.*(1-hm1))) .* (hm1 .> 0.5); 
          endif;
          hm1 = (reshape((hm1[11:rows(hm1)])',nobs,nrep))';
          hm = hm~hm1;
          h=h+1;
     endo;

     print "Finished Halton sequences.";

     if SAVH; 
         save ^HMNAME = hm; 
         print "Saved Halton sequences.";
     endif;

   else;
      print "HALT must equal 1 or 2. Job terminated.";
      stop;

   endif;

  print "Number of rows in Halton matrix: " rows(hm);
  print "Number of columns in Halton matrix: " cols(hm);
endif;

@ END OF HALTON DRAWS @

@ IDA is a vector that identifies the variables with normal, uniform or triangular coefficients.@
@ It is used in LL, gradient and forecst procs. @
  IDA={0};
  if NNC>0; IDA=IDNC;  endif;
  if NNC==0 and NUC>0; IDA=IDUC; endif;
  if NNC>0 and NUC>0; IDA=IDA|IDUC; endif;
  if (NNC+NUC)==0 and NTC>0; IDA=IDTC; endif; 
  if (NNC+NUC)>0 and NTC>0; IDA=IDA|IDTC; endif;  

@ Initialize the STEP to twice its value for Paul Ruud's routine. @
if (OPTIM==1);
  STEP = STEP*2;
endif;


@ Do forecasting @
@ Forecasting procedure modified by Garrett Glasgow, 3/9/00 @
if FCAST;
  if SESHARES;
  Bdraws = rndmn(B,vc,NSIMS);
  q = 1;
  betanew = {};
  do while q <= NSIMS;
   beta = forecst((Bdraws[q,.])',XMAT);
   betanew = betanew | beta';
   q = q+1;
  endo;
  betamean = meanc(betanew);
  betase = stdc(betanew);
  print "Mean forecasted shares for each alternative:";
  print betamean;
  print "Standard Deviations of shares for each alternative:";
  print betase;
  stop;
 else;
  beta = forecst(B,XMAT);
  print "Forecasted shares for each alternative:";
  print beta;
  stop;
 endif;
endif; 

@ Set up and do maximization @
if OPTIM == 1;
  beta = domax(&ll,&gr,B,BACTIVE); 
  print;
  print "Remember: Signs of standard deviations are irrelevant.";
  print "Interpret them as being positive.";
  print;
endif;
 
if OPTIM == 2;
  library maxlik,pgraph;
  #include maxlik.ext;
  maxset;
  _max_GradTol = EPS;
  _max_GradProc = &gr; 
  _max_MaxIters = NITER;
  _max_Algorithm = METHOD;
@ Modified by Garrett Glasgow, 3/9/00 @
@ Default value of _max_CovPar (0) gives imprecise estimates. @
  if SAVECOV;
   _max_CovPar = 1;
  endif;
  if CZVAR;
     _max_Active = BACTIVE;
  endif;
  if DOWT;
     __weight = WEIGHT;
  endif;
  {beta,f,g,cov,ret} = maxlik(XMAT,0,&ll,B);
   call maxprt(beta,f,g,cov,ret);
@ Modified by Garrett Glasgow, 3/9/00 @
@ Saves covariance matrix if SAVECOV=1 @
if SAVECOV;
 save ^COVMAT = cov;
endif;
  print;
  print "Remember: Signs of standard deviations are irrelevant.";
  print "Interpret them as being positive.";
  print;
  if (CZVAR == 0);
      print "gradient(hessian-inverse)gradient is:" ((g/_max_FinalHess)'g);
    else;
      g = selif(g,BACTIVE);
      print "gradient(hessian-inverse)gradient is:" ((g/_max_FinalHess)'g);
  endif;
  if PRTHESS;
    print "diagonal of hessian:" ((diag(_max_FinalHess))');
    print;
  endif; 
endif; 


@ THIS IS THE END OF THE MAIN PROGRAM. @
@ PROCS ll, gr, gpnr, fcast, expand, domax, pcheck follow.@
/* LOG-LIKELIHOOD FUNCTION */
proc ll(b,x);

  @ Relies on the globals: CENSOR, CEVAR, EQMAT, IDCENSOR, IDDEP,  @
  @      IDFC, IDLC, NALT, NECOL, NFC, NLC, NNC, NUC, NTC, NOBS, NREP, @
  @      SEED1, XMAT,IDA                                               @

  local c, k, r, y, km, bn;
  local v, ev, p0, err, seed2,mm; 

  if CEVAR;
    bn = EQMAT' * b;            @ Expand b to its original size. @
  else;
    bn = b;
  endif;

  v = zeros(NOBS,NALT);		@ Argument to logit formula @
 
  p0 = zeros(NOBS,1);		@ Scaled simulated probability	@
  y = (IDDEP-1)*NALT;           @ Identifies dependent variable @
  if CENSOR; 
    c = (IDCENSOR-1)*NALT;      @ Identifies censor variable @
  endif;

  seed2=SEED1;

  k = 1;
  do while k <= NFC;           @ Adds variables with fixed coefficients @
    km = (IDFC[k,1]-1)*NALT;
    v = v + bn[k] .* XMAT[.,(km+1):(km+NALT)];
    k = k+1;
  endo;

  r = 1;
  do while r <= NREP;          @ Repetitions for random coefficients. @
    ev = v;                   
    if DRAWS == 1 ; 
                err = rndns(NOBS,NECOL,seed2);
                if NUC > 0;
                      err[.,(NNC+1):(NNC+NUC)] = (rndus(NOBS,NUC,seed2) .* 2) -1;
                endif;                      
                if NTC > 0;
                     mm = rndus(NOBS,NTC,seed2) ;
                     err[.,(NNC+NUC+1):(NNC+NUC+NTC)] = (sqrt(2.*mm)-1).*(mm .<= 0.5) 
                                                   + (1-sqrt(2.*(1-mm))) .* (mm .> 0.5); 
                endif;
    endif;
    if DRAWS == 2 ; 
               err = hm[r,.];
               err= (reshape(err,NECOL,NOBS))';
    endif;

  @ ERR has NOBS rows and NECOL columns for each repetition. @ 

    k = 1;   

    do while k <= NNC+NUC+NTC;         @ Adds variables with normal, uniform, and triangular coefficients @
      km = (IDA[k,1]-1)*NALT;
      ev = ev + (bn[NFC+(2*k)-1] + (bn[NFC+(2*k)] .* err[.,k]))
                   .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;


    k = 1;
    do while k <= NLC;         @ Adds variables with log-normal coefficients @
      km = (IDLC[k,1]-1)*NALT;
      ev = ev + exp(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)-1]
                      +(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)] .* err[.,(NNC+NUC+NTC+k)]))
                      .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;

    ev = exp(ev);

    if CENSOR;
         p0 = p0 + (sumc((ev .* XMAT[.,(y+1):(y+NALT)])')) 
                ./ (sumc((ev .* XMAT[.,(c+1):(c+NALT)])'));
    else; 
         p0 = p0 + (sumc((ev .* XMAT[.,(y+1):(y+NALT)])')) 
                ./ (sumc(ev'));
    endif;
    r = r+1;
  endo;

  retp(ln(p0./NREP));
endp;

/* GRADIENT */
proc gr(b,x);

  @ Relies on the globals: CENSOR, CEVAR, EQMAT, IDCENSOR, IDDEP,  @
  @      IDFC, IDLC, NALT, NECOL, NFC, NLC, NNC, NUC,NTC,NOBS, NREP, @
  @      SEED1, XMAT,IDA                                               @

  local i, j, k, r, km, l, part, bn;
  local v, ev, p1, p0, denom, der, err, seed2,nn,mm;

  if CEVAR;
    bn = EQMAT' * b;            @ Expand b to its original size. @
  else;
    bn = b;
  endif;

  v = zeros(NOBS,NALT);         @ Argument to logit formula    @

  p0 = zeros(NOBS,1);           @ Scaled simulated probability	@
  der = zeros(NOBS,NFC+(2*(NNC+NUC+NTC+NLC))); @ Derivatives of probabilities	@
  i = (IDDEP-1)*NALT;           @ Identifies dependent variable @
  if CENSOR; 
   j = (IDCENSOR-1)*NALT;       @ Identifies censor variable @
  endif;

  seed2=SEED1;

  k = 1;
  do while k <= NFC;            @ Adds variables with fixed coefficients @
    km = (IDFC[k,1]-1)*NALT;
    v = v + bn[k] .* XMAT[.,(km+1):(km+NALT)];
    k = k+1;
  endo;

  r = 1;
  do while r <= NREP;           @ Repetitions for random coefficients @
    ev = v;
    if DRAWS == 1 ; 
                err = rndns(NOBS,NECOL,seed2);
                if NUC > 0;
                      err[.,(NNC+1):(NNC+NUC)] = (rndus(NOBS,NUC,seed2) .* 2) -1;
                endif;                      
                if NTC > 0;
                     mm = rndus(NOBS,NTC,seed2) ;
                     err[.,(NNC+NUC+1):(NNC+NUC+NTC)] = (sqrt(2.*mm)-1).*(mm .<= 0.5) 
                                                   + (1-sqrt(2.*(1-mm))) .* (mm .> 0.5); 
                endif;
    endif;
    if DRAWS == 2 ; 
               err = hm[r,.];
               err= (reshape(err,NECOL,NOBS))';
    endif;

    @ ERR has NOBS rows and NECOL columns for each repetition. @


    k = 1; 
    do while k <= (NNC+NUC+NTC);         @ Adds variables with normal, uniform and triangular coefficients @
      km = (IDA[k,1]-1)*NALT;
      ev = ev + (bn[NFC+(2*k)-1] + (bn[NFC+(2*k)] .* err[.,k]))
                   .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;

    k = 1;
    do while k <= NLC;         @ Adds variables with log-normal coefficients @
      km = (IDLC[k,1]-1)*NALT;
      ev = ev + exp(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)-1]
                      +(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)] .* err[.,(NNC+NUC+NTC+k)]))
                      .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;

    ev = exp(ev);

    if CENSOR;
      denom = sumc( (ev .* XMAT[.,(j+1):(j+NALT)])' );
    else;
      denom = sumc( ev' );
    endif;
    p1 = sumc((ev .* XMAT[.,(i+1):(i+NALT)])') ./ denom;
    p0 = p0 + p1;

    if CENSOR;
      ev = (ev ./ denom) .* XMAT[.,(j+1):(j+NALT)];
    else;
      ev = (ev ./ denom);
    endif;

    @ Calculate grad for first NFC parameters @

    k = 1;
    do while k <= NFC;
      km = (IDFC[k,1]-1)*NALT;     
      part = (XMAT[.,(i+1):(i+NALT)] - ev) 
                .* XMAT[.,(km+1):(km+NALT)];
      der[.,k] = der[.,k] + (sumc(part') .* p1);
      k = k+1;
    endo;
   
    @ Calculate grad for next 2*(NNC+NUC+NTC) parameters @

    k = 1;
    do while k <= NNC+NUC+NTC;
      km = (IDA[k,1]-1)*NALT;
      part = (XMAT[.,(i+1):(i+NALT)] - ev) 
                 .* XMAT[.,(km+1):(km+NALT)];
      der[.,NFC+(2*k)-1] = der[.,NFC+(2*k)-1] + (sumc(part') .* p1);

      part = (XMAT[.,(i+1):(i+NALT)] - ev) 
                 .* (ERR[.,k] .* XMAT[.,(km+1):(km+NALT)]);
      der[.,NFC+(2*k)] = der[.,NFC+(2*k)] + (sumc(part') .* p1);
      k = k + 1;
    endo;

    @ Calculate grad for next 2*NLC parameters @

    k = 1;
    NN=NNC+NUC+NTC;
    do while k <= NLC;
      km = (IDLC[k,1]-1)*NALT;
      part = (XMAT[.,(i+1):(i+NALT)] - ev) 
       .* exp(bn[NFC+(2*NN)+(2*k)-1]+(bn[NFC+(2*NN)+(2*k)] .* ERR[.,(NN+k)]))
                  .* XMAT[.,(km+1):(km+NALT)];
      der[.,NFC+(2*NN)+(2*k)-1] = der[.,NFC+(2*NN)+(2*k)-1] 
                 + (sumc(part') .* p1);

      part = (XMAT[.,(i+1):(i+NALT)] - ev)
       .* exp(bn[NFC+(2*NN)+(2*k)-1]+(bn[NFC+(2*NN)+(2*k)] .* ERR[.,(NN+k)]))
                  .* (ERR[.,(NN+k)] .* XMAT[.,(km+1):(km+NALT)]);
      der[.,NFC+(2*NN)+(2*k)] = der[.,NFC+(2*NN)+(2*k)] 
                 + (sumc(part') .* p1);
      k = k + 1;
    endo;
   
    r = r+1;
  endo;

  if CEVAR;
    retp((der ./ p0) * EQMAT' );
     else;
    retp(der ./ p0);     
  endif;

endp;

/* GRADIENT FOR PAUL RUUD'S ROUTINE WHEN USING NEWTON-RAPHSON*/
@ USE WHEN OPTIM == 1 AND METHOD == 2 @
proc gpnr(b);
  @ Relies on globals: XMAT  @
  local grad;
  grad = gr(b,XMAT);
  if DOWT;
     retp(sumc(WEIGHT.*grad));
  else;
     retp(sumc(grad));
  endif;
endp;


/* FORECASTED SHARES */
proc forecst(b,x);
  @ Relies on the globals: DOWGT, CENSOR, CEVAR, EQMAT, IDCENSOR,  @
  @      IDFC, IDLC, NALT, NECOL, NFC, NLC, NNC, NUC,NTC,NOBS, NREP, @
  @      SEED1, WEIGHT, XMAT,IDA                                          @

  @ Relies on the globals: NOBS, NALT, NFC, NNC, NLC, IDFC, IDNC, IDLC, @
  @                         NREP, SEED1, FCAST, DOWT, WEIGHT, NECOL   	@

  local i, j, k, r, km, bn;
  local v, ev, p0, err, seed2,mm; 

  if CEVAR;
    bn = EQMAT' * b;            @ Expand b to its original size. @
  else;
    bn = b;
  endif;

  v = zeros(NOBS,NALT);		@ Argument to logit formula @
 
  p0 = zeros(NOBS,NALT);	@ Scaled simulated probability	@
  if CENSOR; 
   j = (IDCENSOR-1)*NALT;       @ Identifies censor variable @
  endif;

  seed2=SEED1;

  k = 1;
  do while k <= NFC;           @ Adds variables with fixed coefficients @
    km = (IDFC[k,1]-1)*NALT;
    v = v + bn[k] .* XMAT[.,(km+1):(km+NALT)];
    k = k+1;
  endo;

  r = 1;
  do while r <= NREP;          @ Repetitions for random coefficients. @
    ev = v;    
               
    if DRAWS == 1 ; 
                err = rndns(NOBS,NECOL,seed2);
                if NUC > 0;
                      err[.,(NNC+1):(NNC+NUC)] = (rndus(NOBS,NUC,seed2) .* 2) -1;
                endif;                      
                if NTC > 0;
                     mm = rndus(NOBS,NTC,seed2) ;
                     err[.,(NNC+NUC+1):(NNC+NUC+NTC)] = (sqrt(2.*mm)-1).*(mm .<= 0.5) 
                                                   + (1-sqrt(2.*(1-mm))) .* (mm .> 0.5); 
                endif;
    endif;
    if DRAWS == 2 ; 
               err = hm[r,.];
               err= (reshape(err,NECOL,NOBS))';
    endif;

    k = 1;
  
   do while k <= (NNC+NUC+NTC);         @ Adds variables with normal, uniform and triangular coefficients @
      km = (IDA[k,1]-1)*NALT;
      ev = ev + (bn[NFC+(2*k)-1] + (bn[NFC+(2*k)] .* err[.,k]))
                   .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;

    k = 1;
    do while k <= NLC;         @ Adds variables with log-normal coefficients @
      km = (IDLC[k,1]-1)*NALT;
      ev = ev + exp(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)-1]
                      +(bn[NFC+(2*(NNC+NUC+NTC))+(2*k)] .* err[.,(NNC+NUC+NTC+k)]))
                      .* XMAT[.,(km+1):(km+NALT)];
      k = k+1;
    endo;

    ev = exp(ev);

    if CENSOR;
         p0 = p0 + (ev .* XMAT[.,(j+1):(j+NALT)]) 
                ./ (sumc((ev .* XMAT[.,(j+1):(j+NALT)])'));
    else; 
         p0 = p0 + ev ./ (sumc(ev '));
    endif;
    r = r+1;
  endo;

  if DOWT;
    retp(meanc(WEIGHT .* (p0./NREP)));
  else;
    retp(meanc(p0./NREP));
  endif;
endp;

/* EXPANDS THE DIRECTION VECTOR; ALLOWS PARAMETERS TO STAY AT STARTING	*/
/* VALUES; HELPER PROCEDURE FOR &domax					*/
proc expand( x, e );
    local i,j;
    i = 0;
    j = 0;
    do while i < rows(e);
        i = i + 1;
        if e[i];
            j = j + 1;
            e[i] = x[j];
        endif;
    endo;
    if j/=rows(x); "Error in expand."; stop; endif;
    retp( e );
endp;


/* MAXIMIZATION ROUTINE COURTESY OF PAUL RUUD */
proc domax( &f, &g, b, bactive );

  @ Relies on the globals: CZVAR, EPS, METHOD, NITER, NOBS,  @
  @                        PRTHESS, XMAT, NP, NEVAR          @

  local f:proc, g:proc;
  local direc, first, grad, sgrad, hesh, ihesh, lambda;
  local nb, printer, repeat, step1, wtsq;
  local _biter, _f0, _f1, _fold,  _tol;

  _tol  = 1;
  _biter = 0;
  nb = seqa(1,1,NEVAR);

  _f0 = f( b, XMAT );
  if DOWT;
    _f0 = sumc(WEIGHT.*_f0);
    wtsq = WEIGHT .^ (.5);
  else;
    _f0 = sumc(_f0);
  endif;

  format /m1 /rdt 16,8;
  print; print; print;

  do while (_tol > EPS or _tol < 0) and (_biter < NITER);
    _biter = _biter + 1;

    print "==========================================================================";
    print "          Iteration: " _biter;
    print "          Function:  " _f0;

    grad = g( b, XMAT );
    if (METHOD == 1);
      if DOWT;
        grad = wtsq.*grad;
        hesh = grad'grad;
        grad = wtsq.*grad;
      else;
        hesh = grad'grad;
      endif;     
    else;
      if DOWT;
        grad = WEIGHT .* grad;
      endif;
      hesh = -gradp( &gpnr, b );   @ WEIGHT done in &gpnr @
    endif;
    sgrad = sumc(grad);

    @ Select only the variables that we want to maximize over @
    if CZVAR; 
      sgrad  = selif( sgrad, bactive );
      hesh  = selif( hesh, bactive );
      hesh  = selif( hesh', bactive );
    endif;

    if (det(hesh)==0);
      print "Singular Hessian!";
      print "Program terminated.";
      stop;
    else;
      ihesh = inv(hesh);
      direc = ihesh * sgrad;
    endif;

    _tol   = direc'sgrad;

    if CZVAR;
      direc = expand( direc, bactive);
    endif;

    print "          Tolerance: " _tol;
    print "--------------------------------------------------------------------------";

    if PRTHESS;
      if CEVAR and CZVAR;
        printer = expand(sgrad./NOBS,bactive)~expand(diag(hesh),bactive);
        printer = nb~EQMAT'*b~EQMAT'*printer[.,1]~EQMAT'printer[.,2];
      elseif CEVAR;
        printer = nb~EQMAT'*b~(EQMAT'*sgrad./NOBS)~(EQMAT'*diag(hesh));
      elseif CZVAR;
        printer = nb~b~expand(sgrad./NOBS,bactive)~expand(diag(hesh),bactive);
      else;
        printer = nb~b~(sgrad./NOBS)~(diag(hesh));
      endif;

      print "                             Coefficients             Rel. Grad.               Hessian";

    else;

      if CEVAR and CZVAR;
        printer = expand(sgrad./NOBS,bactive);
        printer = nb~EQMAT'*b~EQMAT'*printer[.,1];
      elseif CEVAR;
        printer = nb~EQMAT'*b~(EQMAT'*sgrad./NOBS);
      elseif CZVAR;
        printer = nb~b~expand(sgrad./NOBS,bactive);
      else;
        printer = nb~b~(sgrad./NOBS);
      endif;

      print "                             Coefficients             Rel. Grad.";

    endif;
    print printer;

    if (_tol >= 0) and (_tol < 1.e-6);
      break;
    elseif _tol < 0;
      direc = -direc;
    endif;

    step1 = STEP;
    lambda = .5;
    repeat = 1;
    first = 1;
    _f1 = _f0;
    steploop:
      step1 = step1 * lambda;
      _fold = _f1;
      if DOWT;
        _f1 = sumc(WEIGHT .* f(b+step1*direc,XMAT));
      else;
        _f1 = sumc( f( b+step1*direc, XMAT ) );
      endif;
    print "--------------------------------------------------------------------------";
      print " Step: " step1;
      print " Function: " _f1;
      if repeat;
        print " Change: " _f1-_f0;
      else;
        print " Change: " _f1-_fold;
      endif;
      if MNR;
        if (step1 < 1.e-5);
          print "Failed to find increase.";
          retp(b);
        elseif (_f1 <= _f0) and (repeat);
          first = 0;
          goto steploop;
        elseif (_f1 > _fold) and (first);
          lambda = 2;
          repeat = 0;
          goto steploop;
        endif;
      else;
        if (step1 < 1.e-5);
          print "Failed to find increase.";
          retp(b);
        elseif (_f1 <= _f0);
          goto steploop;
        endif;
      endif;
      
      if (repeat);
        b = b + step1*direc;
        _f0 = _f1;
      else;
        b = b + .5 * step1 * direc;
        _f0 = _fold;
      endif;

  endo;

  print "==========================================================================";
  print;

  format /m1 /rdt 1,8;
  if (_tol< EPS);
    print "Converged with tolerance:  " _tol;
    print "Function value:  " _f0;
  else;
    print "Stopped with tolerance:  " _tol;
    print "Function value:  " _f1;
  endif;

  print;
  lambda = eig(hesh);
  if lambda>0;
    print "Function is concave at stopping point.";
  else;
    print "WARNING:  Function is not concave at stopping point.";
  endif;
  print;

  if ROBUST == 0 and DOWT == 1; @ Create Hessian and gradient for regular @
                            @ standard errors when have weights. @
    if CZVAR;
      grad = grad'grad;
      grad = selif(grad, BACTIVE);
      grad = selif(grad', BACTIVE);
      ihesh = ihesh*(grad)*ihesh;
    else;
      ihesh = ihesh*(grad'grad)*ihesh;
    endif;
  endif;

  if ROBUST == 1;   @Create gradient and hessian for robust standard errors. @
    
    if METHOD == 1;
      hesh  = - gradp( &gpnr, b ); 
   
      if CZVAR; 
           grad  = selif( grad', bactive )';
           hesh  = selif( hesh, bactive );
           hesh  = selif( hesh', bactive );
       endif;
   
     endif;

     ihesh=inv(hesh);
     ihesh=ihesh*(grad'grad)*ihesh;
     

  print "Uses robust standard errors.";
  else;
  print "Uses non-robust standard errors.";
  endif;





  if CEVAR and CZVAR;
    printer = expand(sqrt(diag(ihesh)), BACTIVE);
    printer = nb~EQMAT'*b~EQMAT'*printer;
  elseif CEVAR;
    printer = nb~EQMAT'*b~(EQMAT'*sqrt(diag(ihesh)));
  elseif CZVAR;
    printer = nb~b~expand(sqrt(diag(ihesh)),BACTIVE);
  else;
    printer = nb~b~sqrt((diag(ihesh)));
  endif;

  format /m1 /rdt 16,8;
  print "      Parameters               Estimates           Standard Errors";
  print "--------------------------------------------------------------------------";
  print printer;
  retp( b );
endp;


/* This proc checks the inputs if VERBOSE=1 */
proc (0) = pcheck;
  local i, j;

  @ Checking XMAT @
  if ( rows(XMAT) /= NOBS);
    print "XMAT has" rows(XMAT) "rows";
    print "but it should have NOBS="  NOBS   "rows.";
    print "Program terminated.";
    stop;
  elseif( cols(XMAT) /= (NVAR*NALT));
    print "XMAT has" cols(XMAT) "columns";
    print "but it should have NVARxNALT= " (NVAR*NALT) "columns.";
    print "Program terminated.";
    stop;
  else;
    print "XMAT has:";
    print "Rows:  " NOBS;
    print "Cols:  " NVAR*NALT ;
    print "Containing" NVAR "variables for " NALT " alternatives.";
    print;
  endif; 

  @ Checking dependent variable @
  if (FCAST == 0);
    if (IDDEP <= 0);
      print "The dependent variable cannot be located";
      print "since you did not set IDDEP to a strictly positive number.";
      print "Program terminated.";
      stop;
    endif;
    if (IDDEP > NVAR);
      print "The dependent variable cannot be located";
      print "since you set IDDEP larger than NVAR.";
      print "Program terminated.";
      stop;
    endif;
    i = (IDDEP-1)*NALT;
    if (sumc(XMAT[.,(i+1):(i+NALT)]') > 1);
      print "The dependent variable does not sum to one";
      print "over alternatives for each observation.";
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Check fixed coefficients @
  if (NFC /= 0);
    if (rows(IDFC) /= NFC);
      print "IDFC has" rows(IDFC) "rows when it should have NFC=" NFC "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(IDFC) /= 1);
      print "Commas are needed between the elements of IDFC.";
      print "Program terminated.";
      stop;
    endif;
    if (1-(IDFC <= NVAR));
      print "IDFC identifies a variable that is not in the data set.";
      print "All elements of IDFC should be <= NVAR, which is " NVAR;
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Check normally distributed coefficients @
  if (NNC /= 0);
    if (rows(IDNC) /= NNC);
      print "IDNC has" rows(IDNC) "rows when it should have NNC=" NNC "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(IDNC) /= 1);
      print "Commas are needed between the elements of IDNC.";
      print "Program terminated.";
      stop;
    endif;
    if (1-(IDNC <= NVAR));
      print "IDNC identifies a variable that is not in the data set.";
      print "All elements of IDNC should be <= NVAR, which is " NVAR;
      print "Program terminated.";
      stop;
    endif;
  endif;

@ Check uniformly distributed coefficients @
  if (NUC /= 0);
    if (rows(IDUC) /= NUC);
      print "IDUC has" rows(IDUC) "rows when it should have NUC=" NUC "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(IDUC) /= 1);
      print "Commas are needed between the elements of IDUC.";
      print "Program terminated.";
      stop;
    endif;
    if (1-(IDUC <= NVAR));
      print "IDUC identifies a variable that is not in the data set.";
      print "All elements of IDUC should be <= NVAR, which is " NVAR;
      print "Program terminated.";
      stop;
    endif;
  endif;

@ Check traingularly distributed coefficients @
  if (NTC /= 0);
    if (rows(IDTC) /= NTC);
      print "IDTC has" rows(IDTC) "rows when it should have NTC=" NTC "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(IDTC) /= 1);
      print "Commas are needed between the elements of IDTC.";
      print "Program terminated.";
      stop;
    endif;
    if (1-(IDTC <= NVAR));
      print "IDTC identifies a variable that is not in the data set.";
      print "All elements of IDTC should be <= NVAR, which is " NVAR;
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Check log-normally distributed coefficients @
  if (NLC /= 0);
    if (rows(IDLC) /= NLC);
      print "IDLC has" rows(IDLC) "rows when it should have NLC=" NLC "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(IDLC) /= 1);
      print "Commas are needed between the elements of IDLC.";
      print "Program terminated.";
      stop;
    endif;
    if (1-(IDLC <= NVAR));
      print "IDLC identifies a variable that is not in the data set.";
      print "All elements of IDLC should be <= NVAR, which is " NVAR;
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Checking censoring variable. @
  if ((CENSOR /= 1) and (CENSOR /= 0));
    print "CENSOR must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;
  if CENSOR;
    if IDCENSOR <= 0;
      print "The censoring variable cannot be located";
      print "since you did not set IDCENSOR to a strictly positive number.";
      print "Program terminated.";
      stop;
    endif;
    if IDCENSOR > NVAR;
      print "The censoring variable cannot be located";
      print "since you set IDCENSOR larger than NVAR.";
      print "Program terminated.";
      stop;
    endif;
    j = (IDCENSOR-1)*NALT;
    i = ((XMAT[.,(j+1):(j+NALT)] .== 1) .OR (XMAT[.,(j+1):(j+NALT)] .== 0) == 1);
    if (1-i);
      print "One or more elements of your censoring variable do not equal 0 or 1.";
      print "Program terminated.";
      stop; 
    endif;
    if (FCAST == 0);
      i = (IDDEP-1)*NALT;
      j = ((XMAT[.,(i+1):(i+NALT)] .AND (XMAT[.,(j+1):(j+NALT)] .== 0)) == 0);
      if (1-j);
        print "Your censoring variable eliminates the chosen alternative";
        print "for one or more observations.";
        print "Program terminated.";
        stop;
      endif;
    endif;
  endif; 
  
  @ Check weights.@
  if ((DOWT /= 1) and (DOWT /= 0));
    print "DOWT must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;
  if DOWT;
    if (cols(WEIGHT) /= 1);
       print "WEIGHT must have one column.";
       print "Program terminated.";
       stop;
    endif;
    if (rows(WEIGHT) /= NOBS);
       print "WEIGHT must have NOBS=" NOBS " rows, but has " rows(WEIGHT) " rows.";
       print "Program terminated.";
       stop;
    endif;
  endif;
    

  @ Check RESCALE @
  if ((PRTHESS /= 1) and (PRTHESS /= 0));
    print "PRTHESS must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;
  if ((RESCALE /= 1) and (RESCALE /= 0));
    print "RESCALE must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;

  @ Check starting values @
  if (rows(B) /= (NFC+2*NNC+2*NUC+2*NTC+2*NLC));
    print "Starting values B has " rows(B) "rows";
    print "when it should have NFC+2*(NNC+NUC+NTC+NLC)= " (NFC+2*NNC+2*NUC+2*NTC+2*NLC) " rows.";
    print "Program terminated.";
    stop;
  endif;
  if (cols(B) /= 1);
    print "Commas needed between the elements of B.";
    print "Program terminated.";
    stop;
  endif;

  @ Check CZVAR @
  if ((CZVAR /= 1) and (CZVAR /= 0));
    print "CZVAR must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;
  if CZVAR;
    if (rows(BACTIVE) /= (NFC+2*NNC+2*NUC+2*NTC+2*NLC));
      print "BACTIVE has " rows(BACTIVE) "rows";
      print "when it should have NFC+2*(NNC+NUC+NTC+NLC)= " (NFC+2*NNC+2*NUC+2*NTC+2*NLC) "rows.";
      print "Program terminated.";
      stop;
    endif;
    if (cols(BACTIVE) /= 1);
      print "Commas needed between the elements of BACTIVE.";
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Check CEVAR @
  if ((CEVAR /= 1) and (CEVAR /= 0));
    print "CEVAR must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;
  if CEVAR;
    if (cols(EQMAT) /= (NFC+2*NNC+2*NUC+2*NTC+2*NLC));
      print "EQMAT has " cols(EQMAT) " columns";
      print "when it should have NFC+2*(NNC+NUC+NTC+NLC)=" (NFC+2*NNC+2*NUC+2*NTC+2*NLC) " columns.";
      print "Program terminated.";
      stop;
    endif;
    if (rows(EQMAT) >= (NFC+2*NNC+2*NUC+2*NTC+2*NLC));
      print "EQMAT has " rows(EQMAT) " rows";
      print "when it should have strictly less than NFC+2*(NNC+NUC+NTC+NLC)=" (NFC+2*NNC+2*NUC+2*NTC+2*NLC) " rows.";
      print "Program terminated.";
      stop;
    endif;
  endif;

 @ Checking NREP @
  if ( NREP <= 0 );
    print "Error in NREP:  must be positive.";
    print "Program terminated.";
    stop;
  endif;

  @ Check FCAST @
  if ((FCAST /= 1) and (FCAST /= 0));
    print "FCAST must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;

  @ Check METHOD and OPTIM @
  if (FCAST == 0);
    if (METHOD < 1);
      print "METHOD must be 1-6.";
      print "Program terminated.";
      stop;
    endif;
    if (OPTIM /= 1) and (OPTIM /= 2);
      print "OPTIM must be 1 or 2.";
      print "Program terminated.";
      stop;
    endif;
    if ((OPTIM == 1) and (METHOD > 2));
      print "Method "  METHOD " is not an option with OPTIM = 1.";
      print "Program terminted.";
      stop;
    endif;
    if ((OPTIM == 2) and (METHOD > 6));
      print "Method " METHOD " is not an option with OPTIM = 2.";
      print "Program terminated.";
      stop;
    endif;
  endif;

  @ Check MNR @
  if ((MNR /= 1) and (MNR /= 0));
    print "MNR must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;

 @ Check STEP @
  if (STEP <= 0);
    print "STEP must be greater than 0.";
    print "Program terminated.";
    stop;
  endif;

@ Check ROBUST @
  if ((ROBUST /= 1) and (ROBUST /= 0));
    print "ROBUST must be 0 or 1.";
    print "Program terminated.";
    stop;
  endif;

@ Check DRAWS @
  if ((DRAWS /= 1) and (DRAWS /= 2));
    print "DRAWS must be 1 or 2.";
    print "Program terminated.";
    stop;
  endif;

@ Check HALT @
  if ((HALT /= 1) and (HALT /= 2));
    print "HALT must be 1 or 2.";
    print "Program terminated.";
    stop;
  endif;





  print "The inputs pass all the checks and seem fine.";
  print;
  retp;
endp;

/* Halton procedure */

/*  Proc to create Halton sequences using the pattern described in Train, "Halton Sequences
for Mixed Logit." The integer n is the length of the Halton sequence that is required, and 
s is the prime that is used in creating the sequence. 

Given the length of the sequence n, the integer k is determined such that s^k<n<s^k+1. That is,
a sequence using s^1 up to s^k is too short, and a sequence using s^1 up to s^(k+1) is too long. 
Using this fact, the proc is divided in two parts to save time and space. 
The first part creates the sequence for s^1 up to s^k. The second
part creates only as much as needed of the additional sequence using s^(k+1). */

proc halton(n,s);
local phi,i,j,y,x,k;
k=floor(ln(n+1) ./ ln(s));    @We create n+1 Halton numbers including the initial zero.@
phi={0};
i=1;
do while i .le k;  
  x=phi;
   j=1;
  do while j .lt s;
     y=phi+(j/s^i);
     x=x|y;
     j=j+1;
  endo;
  phi=x;
  i=i+1;
endo;
 
x=phi;
 j=1;
do while j .lt s .and rows(x) .lt (n+1);  
   y=phi+(j/s^i);
   x=x|y;
   j=j+1;
 endo;

phi=x[2:(n+1),1];  @Starting at the second element gets rid of the initial zero.@
retp(phi);
endp;




/* For the DOS version of Gauss, here is the CDFINVN
   procedure, which is incorporated directly
   in the Windows version as CDFNI */

proc (1) = cdfinvn(p);
    local p0,p1,p2,p3,p4,q0,q1,q2,q3,q4,maskgt,maskeq,sgn,y,
          xp,pn,norms,mask1,mask0,inf0,inf1;

@ constants @


   p0 = -0.322232431088;                   q0 = 0.0993484626060;
   p1 = -1.0;                              q1 = 0.588581570495;
   p2 = -0.342242088547;                   q2 = 0.531103462366;
   p3 = -0.0204231210245;                  q3 = 0.103537752850;
   p4 = -0.453642210148*1e-4;              q4 = 0.38560700634*1e-2;

@ Main body of code @

  if not (p le 1.0 and p ge 0.0);
     errorlog("error: Probability is out of range.");
     retp(" ");
     end;
  endif;

/* Create masks for p = 0 or p = 1 */

   mask0 = (p .== 0);
   mask1 = (p .== 1);
   inf0 = missrv(miss(mask0,1),-1e+300);
   inf1 = missrv(miss(mask1,1),1e+300);

@ Create masks for handling p > 0.5 and p >= 0.5 @

   maskgt = (p .> 0.5);
   maskeq = (p .ne 0.5);
   sgn = missrv(miss(maskgt,0),-1);

@ Convert p > 0.5 to 1-p @
  pn = (maskgt-p).*sgn+mask1+mask0;   clear maskgt;

@ Computation of function for p < 0.5 @

  y=sqrt(abs((-2*ln(pn))));     clear pn;

  norms = y + ((((y*p4+p3).*y+p2).*y+p1).*y+p0)./
          ((((y*q4+q3).*y+q2).*y+q1).*y+q0);   clear y;
@ Convert results for p > 0.5 and p = 0.5 @

  norms=((norms.*sgn).*maskeq).*(1-mask0).*(1-mask1)+mask0.*inf0+mask1.*inf1;
  retp(norms);
endp;

/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** random (unstandardized) multivariate normals
**
** y = rndmn(mu,vc,n);
**
** inputs: mu = kx1 means
**         vc = kxk variance matrix
**          n = number of simulations
**
** output:  y = nxk matrix of dependent Multivariate Normal Random
Variables
**              each row of y is one 1xk simulation
**
** example:
**
**      c=rndn(30,5);
**      vc=c'c;                 @ some theoretical var-cov matrix,
**                                c'c for the example to assure vc ispos
def @
**      mu=0|5|-10|130|3;
**      y=rndmn(mu,vc,500);
**      "the theoretical correlation matrix";;
**          d=1/sqrt(diag(vc));   d.*vc.*d';?;
**      "simulated correlation matrix";;  corrx(y);
**      "theoretical mean matrix: ";;     mu';
**      "simulated mean matrix:   ";;     meanc(y)';
**
**  History:  
**  12/6/94  Added capability to do cholsky decomposition on matrices 
**           that have symmetric columns and rows of all zeros -- e.g.,
**           the vc matrix returned from a restricted regression.  Also,
**           vectorized computation of result.  Curt Signorino 
*/
proc rndmn(mu,vc,n);
    local k,c,r,i,t,vcd,ad,a,res;
    k=rows(mu);
    c=cols(mu);
    r=rows(vc);
    if ((r/=k).or(cols(vc)/=k)).and(r/=1);
        errorlog "rndmn: mu must be kx1, and vc kxk or scalar"; 
        end;
    endif;
    if n<1; 
        errorlog "rndmn: number of simulations must be >=1   "; 
        end;
    endif;
    if c/=1 and c/=n;
        errorlog "rndmn: mu must be kxn or kx1";
        end;
    endif;

    if vc==0; 
        retp(mu'.*ones(n,1)); 
    endif;
    i=sumc(dotfeq(vc,0)).==r;   @ which columns are all zeros?  @
    if sumc(i)==0;              @ no all-zero columns/rows      @
        a=chol(vc)';            @ matrix square root function   @
    else;                       @ all-zero columns/rows exist   @
        t=delif(eye(r),i);      @ create transform matrix       @
        vcd=t*vc*t';            @ create nonsingular submatrix  @
        ad=chol(vcd);           @ cholsky decomp of submatrix   @
        a=t'ad*t;               @ rebuild full square-root matrix @
    endif;
    res=(mu+a*rndn(k,n))';      @ dep ran normals with mean mu, var vc @
    retp(res);
endp;
