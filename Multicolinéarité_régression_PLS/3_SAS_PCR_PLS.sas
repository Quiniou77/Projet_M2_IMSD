/******************************************************************/
/************* IMPORTATION DU FICHIER DE DONNEES ******************/
/******************************************************************/


libname pls "C:\Users\lahat\Documents\M2IMSD\PLS\Projet_PLS_IMSD_2019_2020";

proc import datafile="C:\Users\lahat\Documents\M2IMSD\PLS\Projet_PLS_IMSD_2019_2020\rlts_pres_2017_ratio.csv"
out=pls.data
dbms=csv 
replace;
getnames=yes;
run;

/******************************************************************/
/********************** QUESTION 1*********************************/
/******************************************************************/


/*Détection de valeurs atypiques**/
data pls.dat (keep=  departement nom_dept nom_departement le_pen_1 macron_1 le_pen_2 macron_2 melenchon fillon hamon 
dupont_aignan  poutou asselineau lassale arthaud cheminade abstentions_1 
 blancs_nuls_1 abstentions_2 blancs_nuls_2);
set pls.data;
nom_departement="nom_dept";
run;

/*Détection de valeurs abérrantes*/

%macro BOXPLOT(var); 
PROC BOXPLOT DATA = pls.dat ;
PLOT &var. / BOXSTYLE = SCHEMATICID horizontal ;
ID departement ;
RUN ;
%mend; 
%BOXPLOT(ABSTENTIONS_1*nom_departement) 
%BOXPLOT(ABSTENTIONS_2*nom_departement) 
%BOXPLOT(blancs_nuls_1*nom_departement) 
%BOXPLOT(blancs_nuls_2*nom_departement) 
%BOXPLOT(le_pen_1*nom_departement) 
%BOXPLOT(le_pen_2*nom_departement) 
%BOXPLOT(macron_1*nom_departement) 
%BOXPLOT(macron_2*nom_departement) 
%BOXPLOT(melenchon*nom_departement) 
%BOXPLOT(fillon*nom_departement) 
%BOXPLOT(hamon*nom_departement) 
%BOXPLOT(poutou*nom_departement) 
%BOXPLOT(dupont_aignan*nom_departement)
%BOXPLOT(asselineau*nom_departement) 
%BOXPLOT(lassale*nom_departement)
%BOXPLOT(arthaud*nom_departement) 
%BOXPLOT(cheminade*nom_departement)


/*Garder que Métropôle*/

data pls.dat_metropole;
     set pls.dat;
	 if departement in(99,201,202,971,972,973,974,975,976,977,986,987,988)
	    then delete;
run;


/******************************************************************/
/********************** QUESTION 3*********************************/
/******************************************************************/


/************* Macro SAS pour obtenir les aides à l'intrétation *************/

/* DATA = Table SAS des données (obligatoire)                               */
/*                                                                          */
/* RESPONSE = Liste des variables à expliquer (obligatoire)                 */
/*                                                                          */
/* VAR = Liste des variables candidates à l'explication (obligatoire)       */
/*                                                                          */
/* NFAC = Nombre de composantes PLS à extraire (par défaut = 2)             */
/*                                                                          */
/* METHOD = PLS ou PCR (par défaut = PLS)                                   */
/*                                                                          */
/* PROB = Probabilité critique pour le RStudent (par défaut = 0.025         */
/*                                                                          */
/* ID = identifiant des individus (obligatoire)                             */
/*                                                                          */
/****************************************************************************/

%macro aide_interpretation(data=,response=,var=,nfac=2,method=PLS,prob=0.025,id=);

%let p=1;
%let var_1=%scan(&var,&p,%str( ));		  
%do %while(&&var_&p ne);
    %let p=%eval(&p+1);
    %let var_&p=%scan(&var,&p,%str( ));
%end;
%let p=%eval(&p-1);

%let q=1;
%let response_1=%scan(&response,&q,%str( ));		  
%do %while(&&response_&q ne);
    %let q=%eval(&q+1);
    %let response_&q=%scan(&response,&q,%str( ));
%end;
%let q=%eval(&q-1);

data _data;
     set &data nobs=n;
	 call symput('n',compress(n));
run;

proc pls data=&data method=&method details nfac=&nfac;
     model &response=&var / solution;
	 output out=_out_pls xscore=t_ yscore=u_ predicted=%do k=1 %to &q;
                                                           pred_&&response_&k
											           %end;;
	 ods output xweights=_xweights;
run;
quit;


/******************/
/* Calcul des VIP */
/******************/

proc corr data=_out_pls outp=_corr noprint;
     var t_1-t_&nfac;
	 with &response;
run;

data _corr;
     set _corr;
	 array t_(&nfac) t_1-t_&nfac;
	 array corr2_(&nfac) corr2_1-corr2_&nfac;
	 if _type_ eq 'CORR';
	 redon_1=0;
     do l=1 to &nfac;
	    corr2_(l)=t_(l)**2;
        redon_1=redon_1+t_(l)**2;
	 end;
	 merg=1;
run;

proc transpose data=_xweights out=_trans prefix=w_c_;
     var &var;
run;

data _trans;
     set _trans;
	 merg=1;
run;

data _vip;
     merge _trans _corr(drop=_name_);
	 by merg;
run;

data _vip;
     set _vip(rename=(_name_=variable));
	 array corr2_(&nfac) corr2_1-corr2_&nfac;
     array w_c_(&nfac) w_c_1-w_c_&nfac;
	 cum_rond=0;
	 do l=1 to &nfac;
        cum_rond=cum_rond+corr2_(l)*w_c_(l)**2;
	 end;
	 vip=sqrt((&p/redon_1)*cum_rond);
	 num_var=_n_;
run;

data _annot;
     set _vip;
	 length text $ 16;
	 x=num_var;
	 y=vip;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=variable;
	 position='2';
	 style='centb';
	 color='red';
run;

%let max_haxis=%eval(&p+1);

symbol1 v=dot i=needle c=blue;

proc gplot data=_vip;
title 'Variables Importance';
     plot vip*num_var / anno=_annot vref=1 haxis=0 to &max_haxis noframe wvref=2;
run;
quit;

proc sort data=_vip;
     by descending vip;
run;

proc print data=_vip;
title 'Variables Importance';
     var vip;
	 id variable;
	 format vip 7.3;
run;


/******************************/
/* Calcul des T2 de Hotelling */
/******************************/

%do k=1 %to &q;

proc means data=_out_pls noprint;
     var &&response_&k pred_&&response_&k;
	 output out=_means min=min_&&response_&k min_pred_&&response_&k max=max_&&response_&k max_pred_&&response_&k;
run;

data _null_;
     set _means;
	 call symput("min_&&response_&k",min(min_&&response_&k,min_pred_&&response_&k));
	 call symput("max_&&response_&k",max(max_&&response_&k,max_pred_&&response_&k)+(max(max_&&response_&k,max_pred_&&response_&k)-min(min_&&response_&k,min_pred_&&response_&k))/10);
	 call symput("pas_&&response_&k",(max(max_&&response_&k,max_pred_&&response_&k)-min(min_&&response_&k,min_pred_&&response_&k))/10);
run;

%end;

proc transpose data=_xweights out=_trans prefix=w_c_;
     var &var;
run;

proc means data=_out_pls noprint;
     var %do h=1 %to &nfac;
             t_&h
         %end;;
	 output out=_means var=%do h=1 %to &nfac;
                               s2_&h
                           %end;;
run;

data _null_;
     set _means;
     %do h=1 %to &nfac;
         call symput("s2_&h",s2_&h);
     %end;
run;

data _out_pls;
     set _out_pls;
	 n=&n;
	 t2_i_0=0;
     %do h=1 %to &nfac;
	     %let h1=%eval(&h-1);
	     t2_i_&h=t2_i_&h1+t_&h**2/&&s2_&h;
	     h=&h;
         seuil_&h=(h*(n**2-1))/(n*(n-h))*finv(0.95,h,n-h);
	     call symput("seuil_&h",seuil_&h);
	     p_val_&h=1-probf(t2_i_&h,h,n-h);
	 %end;
	 ind=_n_;
run;

data _null_;
     call symput('pas',round(&n/10,1));
     if round(&n/10,1)*10 lt &n
	    then call symput('max_n',(round(&n/10,1)+0.5)*10);
        else call symput('max_n',round(&n/10,1)*10);
run;

%do h=1 %to &nfac;

data annot;
     set _out_pls;
	 x=ind;
	 y=t2_i_&h;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 size=2;
	 color='blue';
run;

symbol1 i=none v=none;

proc gplot data=_out_pls;
%if &nfac eq 1
    %then %do;
          title "T2 de Hotelling pour la composante &method : t_1";
	%end;
	%else %do;
          title "T2 de Hotelling pour les composantes &method : t_1 à t_&h";
    %end;
     plot t2_i_&h*ind / vref=&&seuil_&h anno=annot haxis=0 to &max_n by &pas;
run; 
quit;

%end;


/*********************************/
/* Calcul des DModX,N et DModY,N */
/*********************************/

proc reg data=_out_pls;
     model &response &var=%do h=1 %to &nfac;
                              t_&h
                          %end;;
	 output out=_reg r=%do k=1 %to &q;
                              f_&k
                       %end;
                       %do j=1 %to &p;
                           e_&j
                       %end;
                       %do k=1 %to &q;
                           l95=l95_&k u95=u95_&k
					   %end;;
run;
quit;

data _reg;
     set _reg(rename=(%do k=1 %to &q;
                          l95_&k=l95bis_&k
                      %end;));
	 array e_(&p) e_1-e_&p;
	 array e2_(&p) e2_1-e2_&p;
	 array f_(&q) f_1-f_&q;
	 array f2_(&q) f2_1-f2_&q;
	 n=&n;
	 p=&p;
	 q=&q;
	 h=&nfac;
	 do i=1 to &p;
	    e2_(i)=e_(i)**2;
	 end;
	 do i=1 to &q;
	    f2_(i)=f_(i)**2;
	 end;
	 v=sqrt(n/(n-h-1));
	 if &response_1 ne .
	 %do k=2 %to &q;
         and &&response_&k ne . 
	 %end;
	    then do;
	         ss_x_i=sum(of e2_1-e2_&p);
             dmod_x_i=sqrt(ss_x_i/(p-h))*v;
		end;
		else do;
         	 dmod_xps_i=sqrt(sum(of e2_1-e2_&p)/(p-h));
			 ss_xsup=sum(of e2_1-e2_&p);
		end;
	 ss_y_i=sum(of f2_1-f2_&q);
     dmod_y_i=sqrt(ss_y_i/q);
	 %do k=1 %to &q;
	     l95_&k=l95bis_&k;
	 %end;
run;

proc means data=_reg noprint;
     var ss_x_i ss_y_i;
	 output out=_means sum=ss_x ss_y
run;

data _null_;
     set _means;
	 n=&n;
	 p=&p;
	 q=&q;
	 h=&nfac;
	 s_X=sqrt(ss_x/((n-h-1)*(p-h)));
	 call symput('s_X',s_X);
	 s_Y=sqrt(ss_y/((n-h-1)*q));
	 call symput('s_Y',s_Y);
run;

data _reg;
     set _reg;
	 dmod_x_i_n=dmod_x_i/&s_X;
     dmod_xps_i=dmod_xps_i/&s_X;
     dmod_y_i_n=dmod_y_i/&s_Y;
run;

data annot;
     set _reg;
	 x=ind;
	 y=dmod_x_i_n;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=join v=none c=blue w=1 l=2;

proc gplot data=_reg;
title "Indice D_mod_X,N pour les variables explicatives avec &nfac composantes &method";
     plot dmod_x_i_n*ind / anno=annot haxis=0 to &max_n by &pas;
run;
quit;

data annot;
     set _reg;
	 x=ind;
	 y=dmod_y_i_n;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=join v=none c=blue w=1 l=2;

proc gplot data=_reg;
title "Indice D_mod_Y,N pour la variable &response avec &nfac composantes &method";
     plot dmod_y_i_n*ind / anno=annot haxis=0 to &max_n by &pas;
run;
quit;


/***********************************************/
/* Calcul des RStudent pour les X's et les Y's */
/***********************************************/

proc standard data=_out_pls m=0 s=1 out=_stand;
     var &var &response;
run;

%do j=1 %to &p;

proc reg data=_stand noprint;
     model &&var_&j=t_1-t_&nfac;
	 output out=_reg_stand rstudent=rstudent;
run;
quit;

data _reg_stand;
     set _reg_stand;
	 _ind=_n_;
	 seuil_inf=tinv(&prob,&n-1-&nfac);
	 call symput('seuil_inf',seuil_inf);
	 seuil_sup=tinv(1-&prob,&n-1-&nfac);
	 call symput('seuil_sup',seuil_sup);
	 label rstudent='Rstudent';
run;

data _annot;
     set _reg_stand;
	 length text $ 4;
	 x=_ind;
	 y=rstudent;
	 xsys='2';
	 ysys='2';
	 position='5';
	 function='label';
	 if abs(rstudent) gt tinv(1-&prob,&n-1-&nfac)
	    then text=compress(&id);
		else text=' ';
	 color='red';
	 style='centb';
	 size=2;
run;

symbol1 i=join v=none c=blue w=2 l=2;

proc gplot data=_reg_stand;
title "RStudent for explanatory variable &&var_&j";
     plot rstudent*_ind / anno=_annot vref=&seuil_inf 0 &seuil_sup wvref=2 cvref=black;
run;
quit;

%end;

%do k=1 %to &q;

proc reg data=_stand noprint;
     model &&response_&k=t_1-t_&nfac;
	 output out=_reg_stand rstudent=rstudent p=pred_&&response_&k;
run;
quit;

data _reg_stand;
     set _reg_stand;
	 ind=_n_;
	 seuil_inf=tinv(&prob,&n-1-&nfac);
	 call symput('seuil_inf',seuil_inf);
	 seuil_sup=tinv(1-&prob,&n-1-&nfac);
	 call symput('seuil_sup',seuil_sup);
	 label rstudent='Rstudent';
run;

data _annot;
     set _reg_stand;
	 length text $ 4;
	 x=ind;
	 y=rstudent;
	 xsys='2';
	 ysys='2';
	 position='5';
	 function='label';
	 if abs(rstudent) gt tinv(1-&prob,&n-1-&nfac)
	    then text=compress(&id);
		else text=' ';
	 color='red';
	 style='centb';
	 size=2;
run;

symbol1 i=join v=none c=blue w=2 l=2;

proc gplot data=_reg_stand;
title "RStudent for response variable &&response_&k";
     plot rstudent*ind / anno=_annot vref=&seuil_inf 0 &seuil_sup wvref=2 cvref=black;
run;
quit;

%end;


/**************************************/
/* Affichage des cartes des individus */
/**************************************/

%if &nfac gt 1
    %then %do;

data annot;
     set _reg;
	 x=t_1;
	 y=t_2;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur les deux premières composantes &method : t_1 et t_2";
     plot t_2*t_1 / anno=annot href=0 vref=0;
run;
quit;

%end;

%if &nfac gt 2
    %then %do;
	
data annot;
     set _reg;
	 x=t_1;
	 y=t_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur la première et la troisième composantes &method : t_1 et t_3";
     plot t_3*t_1 / anno=annot href=0 vref=0;
run;
quit;

data annot;
     set _reg;
	 x=t_2;
	 y=t_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur la deuxième et la troisième composantes &method : t_2 et t_3";
     plot t_3*t_2 / anno=annot href=0 vref=0;
run;
quit;

%end;

%if &q gt 1
    %then %do;

%if &nfac gt 1
    %then %do;

data annot;
     set _reg;
	 x=u_1;
	 y=u_2;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur les deux premières composantes &method : u_1 et u_2";
     plot u_2*u_1 / anno=annot href=0 vref=0;
run;
quit;

%end;

%if &nfac gt 2
    %then %do;
	
data annot;
     set _reg;
	 x=u_1;
	 y=u_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur la première et la troisième composantes &method : u_1 et u_3";
     plot u_3*u_1 / anno=annot href=0 vref=0;
run;
quit;

data annot;
     set _reg;
	 x=u_2;
	 y=u_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_reg;
title "Carte des individus sur la deuxième et la troisième composantes &method : u_2 et u_3";
     plot u_3*u_2 / anno=annot href=0 vref=0;
run;
quit;

%end;
%end;


/**************************************/
/* Affichage des cartes des variables */
/**************************************/

%if &nfac gt 1
    %then %do;

data annot;
     set _trans;
	 length text $ 16;
	 x=w_c_1;
	 y=w_c_2;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_trans;
title "Carte des variables sur les poids w1 et w2 pour le deux premières composantes &method : t_1 et t_2";
     plot w_c_2*w_c_1 / anno=annot vref=0 href=0;
run;
quit;

%end;

%if &nfac gt 2
    %then %do;

data annot;
     set _trans;
	 length text $ 16;
	 x=w_c_1;
	 y=w_c_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_trans;
title "Carte des variables sur les poids w1 et w3 pour la première et la troisième composantes &method : t_1 et t_3";
     plot w_c_3*w_c_1 / anno=annot vref=0 href=0;
run;
quit;

data annot;
     set _trans;
	 length text $ 16;
	 x=w_c_2;
	 y=w_c_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_trans;
title "Carte des variables sur les poids w2 et w3 pour la deuxième et la troisième composantes &method : t_2 et t_3";
     plot w_c_3*w_c_2 / anno=annot vref=0 href=0;
run;
quit;

%end;

proc corr data=_reg outp=_corr;
     with &var;
	 var %do h=1 %to &nfac;
             t_&h
		 %end;;
run;

%if &nfac gt 1
    %then %do;

data annot;
     set _corr;
	 length text $ 16;
	 if _type_ eq 'CORR';
	 x=t_1;
	 y=t_2;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_corr;
title "Carte des variables sur les deux premières composantes &method : t_1 et t_2";
where _type_ eq 'CORR';
     plot t_2*t_1 / anno=annot vref=0 href=0 haxis=-1 to 1 by 0.2 vaxis=-1 to 1 by 0.2;
run;
quit;

%end;

%if &nfac gt 2
    %then %do;

data annot;
     set _corr;
	 length text $ 16;
	 if _type_ eq 'CORR';
	 x=t_1;
	 y=t_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_corr;
title "Carte des variables sur la deuxième et la troisième composantes &method : t_1 et t_3";
where _type_ eq 'CORR';
     plot t_3*t_1 / anno=annot vref=0 href=0 haxis=-1 to 1 by 0.2 vaxis=-1 to 1 by 0.2;
run;
quit;

data annot;
     set _corr;
	 length text $ 16;
	 if _type_ eq 'CORR';
	 x=t_2;
	 y=t_3;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(_name_);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=none v=none c=blue;

proc gplot data=_corr;
title "Carte des variables sur la deuxième et la troisième composantes &method : t_2 et t_3";
where _type_ eq 'CORR';
     plot t_3*t_2 / anno=annot vref=0 href=0 haxis=-1 to 1 by 0.2 vaxis=-1 to 1 by 0.2;
run;
quit;

%end;


/*********************************************************************************/
/* Carte simultanée des variables et des individus sur le premier plan : Biplots */
/*********************************************************************************/

proc standard data=_reg s=1 out=_stand(keep=&id t_1 t_2);
     var t_1 t_2;
run;

proc corr data=_reg outp=_corr;
     with &response &var;
	 var %do h=1 %to &nfac;
             t_&h
		 %end;;
run;

proc contents data=_corr out=_cont noprint;
run;

data _null_;
     set _cont;
	 if name eq '_NAME_'
	    then call symput('_length_',compress(length));
run;

data _stand(keep=_name_ _type_ t_1 t_2);
     set _stand;
	 length _name_ $ &_length_;
	 _name_=compress(put(&id,$&_length_..));
	 _type_='OBS';
run;

data _corr_bis(keep=_name_ _type_ t_1 t_2);
     set _corr;
	 if _type_ eq 'CORR';
	 _type_='VAR';
run;

proc append base=_corr_bis data=_stand force;
run;

data _corr_bis;
     set  _corr_bis;
	 if _n_ le &q+&p
	    then do;
		     t_1=3.5*t_1;
			 t_2=3.5*t_2;
		end;
	 _num=_n_;
run;

data _annot;
     set _corr_bis;
	 length color $ 6 text $ 18;
	 x=t_1;
	 y=t_2;
	 xsys='2';
	 ysys='2';
	 position='5';
	 function='label';
     text=_name_;
	 style='centb';
	 size=1.5;
	 if _n_ le &q
        then color='green';
	 if _n_ gt &q and _n_ le &p+&q
		then color='red';
	 if _n_ gt &p+&q
	    then color='blue';
	 if _type_ eq 'VAR'
	    then do;
		     _cat=1;
		     output;
			 _cat=3;
			 function='draw';
			 color='black';
			 size=2;
			 output;
             _cat=2;
			 function='move';
			 x=0;
			 y=0;
			 output;
		 end;
		 else do;
		      _cat=1;
              output;
		 end;
run;

proc sort data=_annot;
     by _num _cat;
run;

symbol1 i=none v=none;

proc gplot data=_corr_bis;
title 'Carte simultanée des variables et des individus sur le premier plan : Biplots';
     plot t_2*t_1 / anno=_annot href=0 vref=0 whref=2 wvref=2;
run;
quit;
 

/*********************************************************************/
/* Affichage des valeurs observées vs prédites pour chaque réponse Y */
/*********************************************************************/

%do k=1 %to &q;

data _reg;
     set _reg;
	 bissec=pred_&&response_&k;
run;

data annot;
     set _reg end=fin;
	 length color $ 5;
	 x=pred_&&response_&k;
	 y=&&response_&k;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
	 output;
run;

symbol1 i=none v=none c=blue;
symbol2 i=join v=none c=red w=2;

proc gplot data=_reg;
title "Valeurs observées vs valeurs prédites de &&response_&k avec &nfac composantes &method";
     plot (&&response_&k bissec)*pred_&&response_&k / overlay anno=annot
     haxis=&&&&min_&&response_&k to &&&&max_&&response_&k by &&&&pas_&&response_&k 
     vaxis=&&&&min_&&response_&k to &&&&max_&&response_&k by &&&&pas_&&response_&k;
run;
quit;

%end;


/****************************************************************/
/* Affichage des intervalles de prévision pour chaque réponse Y */
/****************************************************************/

%do k=1 %to &q;

data annot;
     set _reg;
	 x=ind;
	 y=&&response_&k;
	 xsys='2';
	 ysys='2';
	 function='label';
	 text=compress(&id);
	 position='5';
	 style='centb';
	 color='blue';
	 size=2;
run;

symbol1 i=join v=none c=red w=2 l=2;
symbol2 i=join v=none c=green w=2 l=1;
symbol3 i=none v=none c=blue;
symbol4 i=join v=none c=red w=2 l=2;

proc gplot data=_reg;
title "Intervalle de prévision à 95% pour &&response_&k avec &nfac composantes &method";
     plot (l95_&k pred_&&response_&k &&response_&k u95_&k)*ind / anno=annot overlay haxis=0 to &max_n by &pas;
run;
quit;

%end;

%mend aide_interpretation;


/*******************************************************/
/************************ *PCR*************************/
/*******************************************************/

title "PCR Macron 2";
proc pls data=PLS.dat_metropole method=pcr details cv=one cvtest(stat=t2);
     model  macron_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1 / solution;
	 output out=titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual=ry_ press=press;
run;
quit;


%aide_interpretation(data=PLS.dat_metropole,response=macron_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1, method=PCR , id=nom_dept, prob=0.025, nfac=4);


title "PCR Le Pen 2";
proc pls data=PLS.dat_metropole method=pcr details cv=one nfac=12 cvtest(stat=t2);
     model  le_pen_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1 / solution;
	 output out=titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual=ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=le_pen_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1, method=PCR, id=nom_dept, prob=0.025, nfac=4);


title "PCR abstention 2";
proc pls data=PLS.dat_metropole method=pcr details  cv=one cvtest(stat=t2);
     model  abstentions_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1 / solution;
	 output out=titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual=ry_ press=press;
run;
quit;


%aide_interpretation(data=PLS.dat_metropole,response=abstentions_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1, method=PCR, id=nom_dept, prob=0.025, nfac=7);

title "PCR blancs_nuls_2";
proc pls data=PLS.dat_metropole method=pcr details  nfac=13 cv=one cvtest(stat=t2);
     model  blancs_nuls_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1 / solution;
	 output out=titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual=ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=blancs_nuls_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1, method=PCR, id=nom_dept, prob=0.025, nfac=3);

/*******************************************************/
/********************PLS********************************/
/*******************************************************/


title "PLS Macron";
proc pls data=PLS.dat_metropole method=pls details cv=one cvtest(stat=t2);
     model  macron_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1 / solution;
	 output out = titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual = ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=macron_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1, method=PLS, id=nom_dept, prob=0.025, nfac=2);

title "PLS Le Pen";
proc pls data=PLS.dat_metropole method=pls details cv=one cvtest(stat=t2);
     model  le_pen_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1 / solution;
	 output out = titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual = ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=le_pen_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1, method=PLS, id=nom_dept, prob=0.025, nfac=2);


title " PLS abstention2";
proc pls data=PLS.dat_metropole method=pls details cv=one cvtest(stat=t2);
     model  abstentions_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1 / solution;
	 output out=titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual=r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual=ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=abstentions_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale 
            arthaud cheminade blancs_nuls_1, method=PLS, id=nom_dept, prob=0.025, nfac=2);


title "PLS Blanc nul";
proc pls data=PLS.dat_metropole method=pls details cv=one cvtest(stat=t2);
     model  blancs_nuls_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1 / solution;
	 output out = titi_pls yscore=u_ xscore=t_ predicted=y_pred 
            xresidual = r_abstentions_1 r_le_pen_1 r_macron_1 r_melenchon r_fillon r_hamon r_dupont_aignan r_poutou 
                        r_asselineau r_lassale r_arthaud r_cheminade r_blancs_nuls_1
            yresidual = ry_ press=press;
run;
quit;

%aide_interpretation(data=PLS.dat_metropole,response=blancs_nuls_2,var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau 
            lassale arthaud cheminade blancs_nuls_1, method=PLS, id=nom_dept, prob=0.025, nfac=2);


/******************************************************************/
/********************** QUESTION 4*********************************/
/******************************************************************/


title "methode pcr 2";
proc pls data=PLS.dat_metropole method=pcr details nfac= cv=one cvtest(stat=t2);
     model macron_2 le_pen_2 abstentions_2 blancs_nuls_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan 
           poutou asselineau lassale arthaud cheminade blancs_nuls_1 / solution;
  output out=pred_titi_regpls_pls2 yscore=u_ xscore=t_ predicted=t_pls2 p_pls2 yresidual=ry_ press=press;
  ods output xweights=xweights xloadings=xloadings censcaleparms=titi_regpls_pls2;
run;

%aide_interpretation(data=PLS.dat_metropole,response=macron_2 le_pen_2 abstentions_2 blancs_nuls_2,
                     var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale arthaud 
                     cheminade blancs_nuls_1, method=PCR, id=departement, prob=0.025, nfac=2);


/******************  pls 2  *************/


title "methode PLS 2";
proc pls data=PLS.dat_metropole method=pls details nfac=13 cv=one cvtest(stat=t2);
     model  macron_2 le_pen_2 abstentions_2 blancs_nuls_2 = abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan
            poutou asselineau lassale arthaud cheminade blancs_nuls_1 / solution;
  output out=pred_titi_regpls_pls2 yscore=u_ xscore=t_ predicted=t_pls2 p_pls2 yresidual=ry_ press=press;
  ods output xweights=xweights xloadings=xloadings censcaleparms=titi_regpls_pls2;
run;


%aide_interpretation(data=PLS.dat_metropole,response=macron_2 le_pen_2 abstentions_2 blancs_nuls_2,
                     var=abstentions_1 le_pen_1 macron_1 melenchon fillon hamon dupont_aignan poutou asselineau lassale arthaud 
                     cheminade blancs_nuls_1, method=PLS, id=departement, prob=0.025, nfac=4);

