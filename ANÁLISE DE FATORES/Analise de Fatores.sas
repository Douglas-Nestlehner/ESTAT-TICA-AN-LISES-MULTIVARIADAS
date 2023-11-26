/* Considere os dados de arrecadação e gastos públicos de 14 municípios
   do estado de São Paulo, sedes de regiões administrativas, com população
   entre 100 e 350 mil habitantes (dados aquivo Gastos_Puliblicos.xls.
   Objetido: AGRUPAR os municípios através de uma análise de fatores.*/

data gastos_publicos;
input Munic $ Pop ICMS IPVA Admin Educ Saude Urban;
cards;
Araca    174.053  37.739 16.508  3.142 31.638 45.046 16.110
Araraq   188.993  70.567 18.474 15.308 33.681 45.558 15.559
Botucatu 113.543  22.101 10.424 16.562 16.151 10.380  9.688
BragPta  133.545  58.791 10.568 18.831 25.758 21.883 13.719
Catand   109.964  25.235 10.601 12.236 28.846 18.187  9.491
Franca   306.194 101.295 21.704 17.251 41.090 63.217 28.455
Guarat   108.237  81.096  7.222  7.446 21.206 20.671 17.391
Itapet   133.340  31.751  8.132 10.280 23.43119.690 11.336
Jau      118.385  22.212 10.472  6.713 21.83 8 10.197 11.699
Limeira  262.189 107.269 23.473 20.157 50.533 46.853 36.527
Marilia  209.321  95.759 16.102 36.135 32.264 79.861 18.885
Piracic  345.210 217.631 36.499 22.081 58.608 75.803 23.579
PresPte  196.468  54.073 17.676 13.105 31.185 51.510 25.810
RioClaro 178.037 161.644 17.297 20.855 28.418 26.047 17.602
;

*AF por componentes principais;
* ;
proc factor data=gastos_publicos nfactors=2 corr rotate=varimax
     method=prin outstat=saida1 out=saida2 score;
var Pop ICMS IPVA Admin Educ Saude Urban;
run;

*AF por verossimilhança;
proc factor data=gastos_publicos nfactors=2 rotate=varimax corr hey
     method=ml outstat=saida3 out=saida4 score;
var Pop ICMS IPVA Admin Educ Saude Urban;
run;

* Comandos para selecionar e transpor as cargas;
data tcargas;
set saida1;
keep _NAME_ Pop ICMS IPVA Admin Educ Saude Urban;
if _TYPE_='PATTERN';
run;
proc transpose data=tcargas out=cargascp;
  var Pop ICMS IPVA Admin Educ Saude Urban;
  id _NAME_;
run;

/* ajuste por componentes principais */
/* gráfico das cargas fatoriais */
proc gplot data=cargascp;
title1 h=1.5 'Loading plot - metodo C.P.';
symbol v=dot cv=darkblue h=1; 
      plot Factor2*Factor1 / grid haxis=-1 to 1 by 0.25
                                  vaxis=-1 to 1 by 0.25
                                  href=0 vref=0;
run;
quit;

* Selecionar os escores de c.p. e v.m e fazer o grafico comparando;
data tcargas;
set saida3;
keep _NAME_ Pop ICMS IPVA Admin Educ Saude Urban;
if _TYPE_='PATTERN';
run;
proc transpose data=tcargas out=cargasml;
  var Pop ICMS IPVA Admin Educ Saude Urban;
  id _NAME_;
run;

/* ajuste por máxima verossimilhança */
/* gráfico das cargas fatoriais */
proc gplot data=cargasml;
title1 h=1.5 'Loading plot - metodo M.V.';
symbol v=dot cv=darkblue h=1; 
      plot Factor2*Factor1 / grid haxis=0 to 1 by 0.25
                                  vaxis=0 to 1 by 0.25;
run;
quit;

data escorescp;
set saida2;
keep Factor1 Factor2;
rename Factor1=Prin1 Factor2=Prin2;
run;
data escoresml;
set saida4;
keep Factor1 Factor2;
rename Factor1=ML1 Factor2=ML2;
run;

data escores;
merge escorescp escoresml;
run;

/* gráfico dos escores para agrupar os municípios */
proc gplot data=escorescp;
title1 h=1.5 'Factor Escores by Principal Componentes';
symbol v=dot cv=darkblue h=1; 
      plot Prin2*Prin1/ grid href=0 vref=0;
run;
quit;

/* análise comparativa para validação da A.F. */
proc gplot data=escores;
title1 h=1.5 'Factor 1 Escores: Maximum Likelihood vs Principal Component';
symbol v=dot cv=darkblue h=1; 
      plot ML1*Prin1 / grid href=0 vref=0;
run;
quit;

/* análise comparativa para validação da A.F. */
proc gplot data=escores;
title1 h=1.5 'Factor 2 Escores: Maximum Likelihood vs Principal Component';
symbol v=dot cv=darkblue h=1; 
      plot Prin2*ML2/ grid href=0 vref=0;
run;
quit;

proc print data=escores;
run;
