/* Loading the dataset */
FILENAME fat URL "https://raw.githubusercontent.com/keijioda/KNNL/main/chap21/CH21PR07.txt";
DATA diets;
    INFILE fat;
    INPUT y x1 x2;/*response y=lipid, x1=age, x2=dietsfat*/
RUN;
/*Anova table*/
/*Probelm 2)1)2) */
proc glm data=diets plots=all;
    title "ANOVA Table for lipids";
    class x1 x2;
    model y = x1|x2;/* fitting a model with main effects and interactions*/
    means x1 x2;/*mean values for x1 adn x2*/
run;
quit;
/*Anova table with multiple comparisons */
/*2)3) Tukey additivity  */
PROC GLM DATA=diets;
    CLASS x1;/* age as categorical*/
    MODEL y = x1;/*fitting the model with only age*/
    MEANS x1 / HOVTEST=BARTLETT;
    LSMEANS x1 / ADJUST=TUKEY CL PDIFF;/*comparing diet fat with tukey adjustment */
    ODS SELECT OverallANOVA Means HOVTest LSMeans Diff;
    TITLE "ANOVA for lipids across age gropus";
RUN;

PROC GLM DATA=diets;
    CLASS x1 x2;/* age and dietfat as categorical*/
    MODEL y = x1 x2;/*fitting the model with only age and dietfat*/
    output out=res R=R P=fitted RSTUDENT=R_STD;
    TITLE "Tukey's Additivity";
RUN;
PROC GLM DATA=res;
    CLASS x1 x2;/* age and dietfat as categorical*/
    MODEL y = x1 x2 fitted*fitted;
RUN;

/*generating interaction plots for age and diet fat*/
proc sgplot data=FatGrid;
    series x=x1 y=LSMean / group=x2 markers;
    xaxis label="Age";
    yaxis label="Estimated Marginal Means (Lipids)";
    title "Interaction Plot of Age and DietFat on Lipids";
run;
/* 2)4)5) */
proc glm data=diets;
    class x1 x2; /* x1: Age (block), x2: DietFat */
    model y = x1 x2; /* Additive model without interaction */
    lsmeans x2 / pdiff adjust=tukey; /* LSMeans and pairwise comparisons for x2 */
    ods output Type3=Type3Results; /* Outputing Type III ANOVA results */
run;
quit;
/* additive model */
proc glm data=diets;
    class x1 x2;
    model y = x1 x2;/*fitting the model with x1 adn x2*/
run;
quit;
/* model without blocking(exclude age)  */
proc glm data=diets;
    class x2;
    model y = x2;
run;
quit;

/*2)6)  */

/* Fitting the model, generating diagnostic outputs */
proc glm data=diets;
    class x1 x2;  /* x1: Age (block), x2: DietFat */
    model y = x1 x2;/* additive model without interaction*/
    output out=diag_data r=residual p=fitted student=student_resid cookd=cooksd;
    /* r: residuals, p: fitted values, student: studentized residuals, cookd: Cook's distance */
run;
quit;

/*plotting Residuals vs. Fitted Values to check variance*/
proc sgplot data=diag_data;
    scatter x=fitted y=residual / markerattrs=(symbol=circlefilled color=blue);
    refline 0 / axis=y lineattrs=(color=red);
    xaxis label="Fitted Values (Ŷij)";
    yaxis label="Residuals (eij)";
    title "Residuals vs. Fitted Values";
run;


/* Q_Q plot for residuals -checking for normality */
proc univariate data=diag_data normal;
    var residual;
    qqplot residual / normal(mu=est sigma=est);
    title "Normal Probability Plot of Residuals";
run;


/* Studentized Residuals vs. Fitted Values- detecting outliers*/
proc sgplot data=diag_data;
    scatter x=fitted y=student_resid / markerattrs=(symbol=circlefilled color=blue);
    refline -3 3 / axis=y lineattrs=(color=red);
    xaxis label="Fitted Values (Ŷij)";
    yaxis label="Studentized Residuals (tij)";
    title "Studentized Residuals vs. Fitted Values";
run;

/* Check for Outliers */
proc print data=diag_data;
    where abs(student_resid) > 3; /* Identifying outliers with |tij| > 3 */
    var x1 x2 y fitted residual student_resid cooksd;
    title "Potential Outliers (|tij| > 3)";
run;


/*2)7)-----------------------------  */
/* Fit the model and output residuals */
proc glm data=diets;
    class x1 x2; /* x1: Age (block), x2: DietFat */
    model y = x1 x2; /* Additive model without interaction */
    output out=diag_data r=residual; /* r: residuals (eij) */
run;
quit;

/* Plot Residuals (eij) vs. Blocks (x1) */
proc sgplot data=diag_data;
    scatter x=x1 y=residual / markerattrs=(symbol=circlefilled color=blue);
    refline 0 / axis=y lineattrs=(color=red);
    xaxis label="Blocks (x1: Age)";
    yaxis label="Residuals (eij)";
    title "Residuals vs. Blocks (x1)";
run;

/* Plot Residuals (eij) vs. Treatments (x2) */
proc sgplot data=diag_data;
    scatter x=x2 y=residual / markerattrs=(symbol=circlefilled color=blue);
    refline 0 / axis=y lineattrs=(color=red);
    xaxis label="Treatments (x2: DietFat)";
    yaxis label="Residuals (eij)";
    title "Residuals vs. Treatments (x2)";
run;

