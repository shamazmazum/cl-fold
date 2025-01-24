# Benchmarks

This is what running benchmarks on my system gives.

## FOLDL

~~~
Folding lists with FOLDL
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      10.853961  0.107794  0.109255  0.108505  0.10854   0.00018    
RUN-TIME         100      10.853929  0.107897  0.109253  0.108504  0.108539  0.000176   
USER-RUN-TIME    100      10.853947  0.107906  0.109257  0.108501  0.108539  0.000175   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      816        0         816       0         8.16      81.19097   
EVAL-CALLS       100      0          0         0         0         0         0.0        
1023320000000
Folding lists with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      39.510944  0.374604  0.440167  0.378611  0.395109  0.026161   
RUN-TIME         100      39.510532  0.374599  0.440162  0.378608  0.395105  0.02616    
USER-RUN-TIME    100      39.510532  0.374581  0.440165  0.378606  0.395105  0.02616    
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      32176      0         32176     0         321.76    3201.4717  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1023320000000
Folding vectors with FOLDL
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      14.558858  0.138591  0.167707  0.145213  0.145589  0.003252   
RUN-TIME         100      14.557776  0.138588  0.167708  0.145212  0.145578  0.003208   
USER-RUN-TIME    100      14.557777  0.138569  0.1677    0.145214  0.145578  0.003209   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      0          0         0         0         0         0.0        
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
Folding vectors with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      55.56217   0.549912  0.5623    0.555901  0.555622  0.002484   
RUN-TIME         100      55.561584  0.549906  0.562293  0.555894  0.555616  0.002484   
USER-RUN-TIME    100      55.561573  0.54991   0.56228   0.555876  0.555616  0.002484   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      31248      0         31248     0         312.48    3109.1367  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
~~~

## FOLDR

~~~
Folding lists with FOLDR
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      38.93931   0.374811  0.412832  0.378244  0.389393  0.016526   
RUN-TIME         100      38.938927  0.374806  0.412823  0.37824   0.389389  0.016527   
USER-RUN-TIME    100      38.938877  0.374819  0.412811  0.378238  0.389389  0.016526   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      32976      0         32976     0         329.76    3281.0706  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1023320000000
Folding lists with REDUCE
-                SAMPLES  TOTAL         MINIMUM     MAXIMUM     MEDIAN      AVERAGE       DEVIATION  
REAL-TIME        100      57.809895     0.527874    0.688427    0.547468    0.578099      0.060259   
RUN-TIME         100      57.797066     0.527885    0.688238    0.547494    0.577971      0.060289   
USER-RUN-TIME    100      57.78923      0.527885    0.688256    0.547434    0.577892      0.060329   
SYSTEM-RUN-TIME  100      0.007823      0           0.007788    0           0.000078      0.000775   
PAGE-FAULTS      100      0             0           0           0           0             0.0        
GC-RUN-TIME      100      301.177       1.829       5.186       2.466       3.01177       1.075021   
BYTES-CONSED     100      128000024896  1279980896  1280048512  1280013648  1280000300.0  16820.895  
EVAL-CALLS       100      0             0           0           0           0             0.0        
1023320000000
Folding vectors with FOLDR
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      12.61985   0.126049  0.12702   0.126178  0.126198  0.000115   
RUN-TIME         100      12.619872  0.126047  0.127018  0.126176  0.126199  0.000114   
USER-RUN-TIME    100      12.619879  0.126048  0.127015  0.126177  0.126199  0.000114   
SYSTEM-RUN-TIME  100      0.000017   0         0.000001  0         0.0       0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      448        0         448       0         4.48      44.575436  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
Folding vectors with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      45.017654  0.430585  0.501398  0.432972  0.450177  0.024491   
RUN-TIME         100      45.01722   0.43058   0.501377  0.432967  0.450172  0.024493   
USER-RUN-TIME    100      45.01722   0.430588  0.501357  0.432967  0.450172  0.024492   
SYSTEM-RUN-TIME  100      0.000003   0         0.000002  0         0.0       0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      31104      0         31104     0         311.04    3094.8088  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
~~~

## FOLDR1

~~~
Folding lists with FOLDR1
-                SAMPLES  TOTAL         MINIMUM     MAXIMUM     MEDIAN      AVERAGE       DEVIATION  
REAL-TIME        100      53.022217     0.453758    0.632069    0.523505    0.530222      0.070662   
RUN-TIME         100      52.99369      0.453802    0.628767    0.522775    0.529937      0.070517   
USER-RUN-TIME    100      52.962406     0.445968    0.628742    0.520456    0.529624      0.070716   
SYSTEM-RUN-TIME  100      0.031419      0           0.015694    0           0.000314      0.001896   
PAGE-FAULTS      100      0             0           0           0           0             0.0        
GC-RUN-TIME      100      619.959       3.628       7.771       5.897       6.19959       0.840594   
BYTES-CONSED     100      254720045824  2547187648  2547254624  2547187680  2547200500.0  16685.865  
EVAL-CALLS       100      0             0           0           0           0             0.0        
1023320000000
Folding lists with REDUCE
-                SAMPLES  TOTAL         MINIMUM     MAXIMUM     MEDIAN      AVERAGE       DEVIATION  
REAL-TIME        100      61.46889      0.528246    0.687412    0.654029    0.614689      0.063593   
RUN-TIME         100      61.4543       0.52801     0.686789    0.654048    0.614543      0.063581   
USER-RUN-TIME    100      61.44644      0.52801     0.686783    0.654047    0.614464      0.063666   
SYSTEM-RUN-TIME  100      0.007882      0           0.007861    0           0.000079      0.000782   
PAGE-FAULTS      100      0             0           0           0           0             0.0        
GC-RUN-TIME      100      323.724       1.821       4.959       2.626       3.23724       1.143905   
BYTES-CONSED     100      127999990224  1279980896  1280013664  1280013648  1279999900.0  16163.57   
EVAL-CALLS       100      0             0           0           0           0             0.0        
1023320000000
Folding vectors with FOLDR1
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      13.6941    0.125849  0.143153  0.142773  0.136941  0.008037   
RUN-TIME         100      13.694099  0.125847  0.143149  0.142771  0.136941  0.008035   
USER-RUN-TIME    100      13.694101  0.125848  0.143143  0.142769  0.136941  0.008035   
SYSTEM-RUN-TIME  100      0.000001   0         0.000001  0         0.0       0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      448        0         448       0         4.48      44.575436  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
Folding vectors with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      53.593433  0.456793  0.566742  0.549957  0.535934  0.02296    
RUN-TIME         100      53.59287   0.456787  0.566736  0.54995   0.535929  0.022958   
USER-RUN-TIME    100      53.59286   0.456787  0.566738  0.549953  0.535929  0.022958   
SYSTEM-RUN-TIME  100      0.000004   0         0.000002  0         0.0       0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      34576      0         34576     0         345.76    3440.2686  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1051560000000
~~~

## MAPFOLDL

~~~
Folding lists with MAPFOLDL
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      12.594631  0.124182  0.249035  0.124673  0.125946  0.012372   
RUN-TIME         100      12.594651  0.124181  0.249197  0.124672  0.125947  0.012388   
USER-RUN-TIME    100      12.594655  0.124182  0.2492    0.124673  0.125947  0.012388   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      0          0         0         0         0         0.0        
EVAL-CALLS       100      0          0         0         0         0         0.0        
966840000000
Folding lists with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      49.856518  0.497907  0.499755  0.498505  0.498565  0.000373   
RUN-TIME         100      49.856155  0.497903  0.49975   0.498499  0.498562  0.000371   
USER-RUN-TIME    100      49.856155  0.497902  0.499746  0.498497  0.498562  0.000371   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      33760      0         33760     0         337.6     3359.0776  
EVAL-CALLS       100      0          0         0         0         0         0.0        
966840000000
Folding vectors with MAPFOLDL
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      14.231296  0.142184  0.143148  0.142272  0.142313  0.000144   
RUN-TIME         100      14.231328  0.142183  0.143148  0.142272  0.142313  0.000145   
USER-RUN-TIME    100      14.231339  0.142188  0.143148  0.142272  0.142313  0.000145   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      29616      0         29616     0         296.16    2946.755   
EVAL-CALLS       100      0          0         0         0         0         0.0        
1012360000000
Folding vectors with REDUCE
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      57.434647  0.533936  0.698314  0.541404  0.574346  0.063513   
RUN-TIME         100      57.434116  0.533929  0.698307  0.541397  0.574341  0.063513   
USER-RUN-TIME    100      57.434128  0.533929  0.698308  0.541394  0.574341  0.063514   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      35024      0         35024     0         350.24    3484.844   
EVAL-CALLS       100      0          0         0         0         0         0.0        
1012360000000
~~~

## MAPFOLDR

~~~
Folding lists with MAPFOLDR
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      42.93274   0.385467  0.44942   0.443627  0.429327  0.020752   
RUN-TIME         100      42.93215   0.385455  0.449416  0.443624  0.429322  0.020752   
USER-RUN-TIME    100      42.932148  0.385456  0.449416  0.443625  0.429321  0.020752   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      4160       0         4160      0         41.6      413.91476  
EVAL-CALLS       100      0          0         0         0         0         0.0        
966840000000
Folding lists with REDUCE
-                SAMPLES  TOTAL         MINIMUM     MAXIMUM     MEDIAN      AVERAGE       DEVIATION  
REAL-TIME        100      70.35314      0.652588    0.825035    0.672579    0.703531      0.061691   
RUN-TIME         100      70.340965     0.652607    0.824607    0.672598    0.70341       0.06164    
USER-RUN-TIME    100      70.199326     0.652607    0.824609    0.672228    0.701993      0.061607   
SYSTEM-RUN-TIME  100      0.141731      0           0.06299     0           0.001417      0.008361   
PAGE-FAULTS      100      0             0           0           0           0             0.0        
GC-RUN-TIME      100      255.573       1.186       19.816      1.919       2.55573       2.384575   
BYTES-CONSED     100      128000221440  1279980896  1280245056  1280013648  1280002200.0  29242.121  
EVAL-CALLS       100      0             0           0           0           0             0.0        
966840000000
Folding vectors with MAPFOLDR
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      14.579842  0.141394  0.150557  0.145581  0.145798  0.003601   
RUN-TIME         100      14.579755  0.141393  0.150557  0.14558   0.145798  0.003604   
USER-RUN-TIME    100      14.579753  0.141394  0.150562  0.145578  0.145798  0.003604   
SYSTEM-RUN-TIME  100      0          0         0         0         0         0.0        
PAGE-FAULTS      100      0          0         0         0         0         0.0        
GC-RUN-TIME      100      0          0         0         0         0         0.0        
BYTES-CONSED     100      448        0         448       0         4.48      44.575436  
EVAL-CALLS       100      0          0         0         0         0         0.0        
1012360000000
Folding vectors with REDUCE
-                SAMPLES  TOTAL     MINIMUM   MAXIMUM   MEDIAN    AVERAGE   DEVIATION  
REAL-TIME        100      58.34333  0.564663  0.669451  0.566436  0.583433  0.037406   
RUN-TIME         100      58.34283  0.564659  0.669445  0.566431  0.583428  0.037408   
USER-RUN-TIME    100      58.34283  0.564659  0.669445  0.566431  0.583428  0.037408   
SYSTEM-RUN-TIME  100      0         0         0         0         0         0.0        
PAGE-FAULTS      100      0         0         0         0         0         0.0        
GC-RUN-TIME      100      0         0         0         0         0         0.0        
BYTES-CONSED     100      31152     0         31152     0         311.52    3099.585   
EVAL-CALLS       100      0         0         0         0         0         0.0        
1012360000000
~~~
