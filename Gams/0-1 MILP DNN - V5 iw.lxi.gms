$title A Transportation Problem (TRNSPORT,SEQ=1)

$onText
Think on how to add the several inputs at the same time.
Maybe adding another dimenssion to the system.

Keywords: linear programming, transportation problem, scheduling
$offText

Set

   in_types         input types of ML model
                 /lsu, lon, lat, tem, pre, rad/
   in_lsu(in_types)  LSU input type
                 / lsu /
   in_env(in_types)  weather input types
                 / lon, lat, tem, pre, rad /


   neurons_lay1    Neurons in layer 1   / N1*N10 /
   neurons_lay2    Neurons in layer 2   / O1*O6 /
   neurons_lay3    Neurons in layer 3   / P1*P3 /

   out_types       output types of ML model   / har /



Parameter input(in_env) input layer
               / lon 1
                 lat 1
                 tem 1
                 pre 1
                 rad 1/;

Parameter bias_lay1(neurons_lay1) bias 1
        /N1  3
         N2  2
         N3  4
         N4  2
         N5  3
         N6  4
         N7  5
         N8  6
         N9  7
         N10 8/;

Parameter bias_lay2(neurons_lay2) bias 2
        /O1  1
         O2  2
         O3  1
         O4  2
         O5  3
         O6  2/ ;

Parameter bias_lay3(neurons_lay3) bias 3
        /P1  2
         P2  2
         P3  1/;


table i_W1(in_types,neurons_lay1) weight 1
           N1       N2       N3       N4       N5       N6       N7       N8       N9      N10
lsu        3        2        4        2        1        2        3        2        1       2
lon        2        2        2        2        2        2        2        2        2       2
lat        2        2        2        2        2        2        2        2        2       2
tem        2        2        2        2        2        2        2        2        2       2
pre        2        2        2        2        2        2        2        2        2       2
rad        2        2        2        2        2        2        2        2        2       2;

table i_W2(neurons_lay1,neurons_lay2) weight 2
          O1         O2         O3         O4         O5         O6
N1        0.3        0.2        0.4        0.2        0.1        0.2
N2        0.2        0.2        0.2        0.2        0.2        0.2
N3        0.2        0.2        0.2        0.2        0.2        0.2
N4        0.2        0.2        0.2        0.2        0.2        0.2
N5        0.2        0.2        0.2        0.2        0.2        0.2
N6        0.2        0.2        0.2        0.2        0.2        0.2
N7        0.2        0.2        0.2        0.2        0.2        0.2
N8        0.2        0.2        0.2        0.2        0.2        0.2
N9        0.2        0.2        0.2        0.2        0.2        0.2
N10       0.2        0.2        0.2        0.2        0.2        0.2;

table i_W3(neurons_lay2,neurons_lay3) weight 3
          P1       P2       P3
O1        3        2        4
O2        2        2        2
O3        2        2        2
O4        2        2        2
O5        2        2        2
O6        2        2        2
;

table i_W4(neurons_lay3,out_types) weight 4
          har
P1        3
P2        2
P3        2
;



Variables
   v_lsu
   input_lsu(neurons_lay1)
   input_env(neurons_lay1)
   z1(neurons_lay1)
   a1(neurons_lay1)
   z2(neurons_lay2)
   a2(neurons_lay2)
   z3(neurons_lay3)
   a3(neurons_lay3)
   output(out_types)
   goal
   ;

*Variable w;
*add the variable and the parameter lsu together

Equations
    input_layer_lsu(neurons_lay1)
    input_layer_env(neurons_lay1)
    layer_1(neurons_lay1)
    activation_1(neurons_lay1)
    layer_2(neurons_lay2)
    activation_2(neurons_lay2)
    layer_3(neurons_lay3)
    activation_3(neurons_lay3)
    output_layer(out_types)

    maxcon(out_types)
    mincon(out_types)

    goal_func
;

*Input Layer: transforms 6 input nodes into 10 neurons at hidden layer 1:
input_layer_lsu(neurons_lay1)..   input_lsu(neurons_lay1) =e= sum(in_lsu,v_lsu*i_W1(in_lsu,neurons_lay1));
input_layer_env(neurons_lay1)..   input_env(neurons_lay1) =e= sum(in_env,input(in_env)*i_W1(in_env,neurons_lay1));
layer_1(neurons_lay1)..           z1(neurons_lay1) =e= input_lsu(neurons_lay1) + input_env(neurons_lay1) + bias_lay1(neurons_lay1);
**activation_1(neurons_lay1)..      a1(neurons_lay1) =e= 1/(1+ exp(-z1(neurons_lay1)));
activation_1(neurons_lay1)..      a1(neurons_lay1) =e= max(0,z1(neurons_lay1));

layer_2(neurons_lay2)..           z2(neurons_lay2) =e= sum(neurons_lay1,a1(neurons_lay1)*i_W2(neurons_lay1,neurons_lay2)) + bias_lay2(neurons_lay2);
**activation_2(neurons_lay2)..      a2(neurons_lay2) =e= 1/(1+ exp(-z2(neurons_lay2)));
activation_2(neurons_lay2)..      a2(neurons_lay2) =e= max(0,z2(neurons_lay2));

layer_3(neurons_lay3)..           z3(neurons_lay3) =e= sum(neurons_lay2,a2(neurons_lay2)*i_W3(neurons_lay2,neurons_lay3)) + bias_lay3(neurons_lay3);
**activation_3(neurons_lay3)..      a3(neurons_lay3) =e= 1/(1+ exp(-z3(neurons_lay3)));
activation_3(neurons_lay3)..      a3(neurons_lay3) =e= max(0,z3(neurons_lay3));

output_layer(out_types)..         output(out_types) =e= sum(neurons_lay3,a3(neurons_lay3)*i_W4(neurons_lay3,out_types));
*output_layer..          output =e= sum(neurons_lay2,z2(neurons_lay2));

maxcon(out_types)..               output(out_types) =l= 5000;
mincon(out_types)..               output(out_types) =g= -5000;

goal_func..                            goal=e= sum(out_types, output(out_types));


Model ANN / all /;

*Option DNLP = Cplex;

solve ANN maximizing goal using DNLP;

display v_lsu.l, goal.l, z1.l, a1.l, z2.l, a2.l ;
