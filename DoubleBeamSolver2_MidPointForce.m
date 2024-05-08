clc; clear;

% From the Bending Moment Diagram of beam 1
% Let 
syms Mb_1; % be the moment at point B (the base)
syms Pa_1; % be the force at point of application,
syms Pe_1; % be the force at the end beam (Point e)
% ta_1 be the applied deformation
% te_1 be the deformation at the end beam
syms La_1; % be the length at the point of application
syms L_1; % be the length of the beam.
% If the modulus of elasticity is 
syms E_1; % and 
syms b_1 d_1; % are the depth and thickness of the beam, assuming a
% rectangular cross section, the moment of area I_1 becomes
I_1 = b_1 * d_1 ^ 3 / 12;
% 
% Sum of moments at B
% Mb_1 - Pa_1 * La_1 + Pe_1 * L_1 == 0; 
Mb_1 = Pa_1 * La_1 - Pe_1 * L_1;
% Since L_1 and La_1 are known, we have three unknows at this point and
% need two more equations to solve the system.

% The rectangles have these areas
A1_1 = La_1 * (Pa_1 - Pe_1); % +1 equations, +1 unknowns
A2_1 = (L_1 - La_1) * Pe_1; % +1 equations, +1 unknowns

% Lets Solve the tringles area and moment of arms for Moment area method
% Let Y1_1 be the height and x1_1 be the base of the first triangle where
% Y1_1 = Mb_1; And from the ratio of the triangles,
% x1_1 / Y1_1 = La_1 / Y3_1, since Y1_1 == Mb_1 and Y3_1 == A1_1
% x1_1 / Mb_1 = La_1 / Y3_1
x1_1 = La_1 / A1_1 * Mb_1; % which is a length
% then the area would be
% A3_1 = 1 / 2 * x1_1 * Y1_1; and 
A3_1 = 1 / 2 * x1_1 * Mb_1; % which is moment x length

% Similarly, for the 2nd Triangle
% Let Y2_1 be the height and x2_1 be the base of the 2nd triangle where
% Y2_1 = Y3_1 - Y1_1; 
% Y2_1 = A1_1 - Mb_1; (eqA_1)
% and the base is
x2_1 = La_1 - x1_1;
% then the area would be
% A4_1 = 1 / 2 * x2_1 * Y2_1, which is
A4_1 = 1 / 2 * x2_1 * (A1_1 - Mb_1);

% and for the third triangle,
% Let Y2_1 be the height and x3_1 be the base of the 2nd triangle where
% Y2_1 = A1_1 - Mb_1; (from eqA_1)
% and the base is
x3_1 = L_1 - La_1;
% then the area would be
% A5_1 = 1 / 2 * x3_1 * Y2_1, which is
A5_1 = 1 / 2 * (L_1 - La_1) * (A1_1 - Mb_1);

% We introduces as much equations as the number of variables and still need
% two more equations to solve the system.

% Lets apply the moment area method Theorem 2 for Point e w.r.t the base
% The deformation is
syms te_1; % or
% teb_1  = te_1 where
eq1_1 = te_1 == 1 / (E_1 * I_1) * ( -A3_1 * (x3_1 + x2_1 + 2/3 * x1_1) ...
                       +A4_1 * (x3_1 + 1/3 * x2_1) ...
                       +A5_1 * (2/3 * x3_1));
% and the deformation at Point a w.r.t the base is:
syms ta_1; % Which is a known quantity
% tab_1 = ta_1
eq2_1 = ta_1 == 1 / (E_1 * I_1) * ( -A3_1 * (x2_1 + 2/3 * x1_1) ...
                       +A4_1 * (1/3 * x2_1));
                   
% eq2_1 brings in one more variable, so it doesnt count in the solution
                   
% Since ta_1 is a known value, we only need one more equation to solve the
% system.




% Now, Lets come the second beam.

% For now, we are assuming that the force at the end point Pe_2 is acting
% in inverse direction. This assumption will make the solution the same for
% the 2nd beam too. We can adjust the direction at the time we combine the
% two beams
% From the Bending Moment Diagram of beam 2 (Beam 1')
% Let 
syms Mb_2; % be the moment at point B (the base)
syms Pa_2; % be the force at point of application,
syms Pe_2; % be the force at the end beam (Point e)
% ta_1 be the applied deformation
% te_1 be the deformation at the end beam
syms La_2; % be the length at the point of application
syms L_2; % be the length of the beam.
% If the modulus of elasticity is 
syms E_2; % and 
syms b_2 d_2; % are the depth and thickness of the beam, assuming a
% rectangular cross section, the moment of area I_2 becomes
I_2 = b_2 * d_2 ^ 3 / 12;

% Sum of moments at B gives
% Mb_2 - Pa_2 * La_2 + Pe_2 * L_2 == 0; 
Mb_2 = Pa_2 * La_2 - Pe_2 * L_2;
% Even with one more equation, since Mb_2 and Pa_2 are two new unknows, we are gonna need one more
% equation (total 2) to solve the sytem. 

% The rectangles have these areas
A1_2 = La_2 * (Pa_2 - Pe_2); % +1 equations, +1 unknowns
A2_2 = (L_2 - La_2) * Pe_2; % +1 equations, +1 unknowns

% Lets Solve the tringles area and moment of arms for Moment area method
% Let Y2_2 be the height and x2_2 be the base of the first triangle where
% Y1_2 = Mb_2; And from the ratio of the triangles,
% x1_2 / Y1_2 = La_2 / Y3_2, since Y1_2 == Mb_2 and Y3_2 == A1_2
% x1_2 / Mb_2 = La_2 / Y3_2
x1_2 = La_2 / A1_2 * Mb_2; % which is a length
% then the area would be
% A3_2 = 1 / 2 * x1_2 * Y1_2; and 
A3_2 = 1 / 2 * x1_2 * Mb_2; % which is moment x length

% Similarly, for the 2nd Triangle
% Let Y2_2 be the height and x2_2 be the base of the 2nd triangle where
% Y2_2 = Y3_2 - Y1_2; 
% Y2_2 = A1_2 - Mb_2; (eqA_2)
% and the base is
x2_2 = La_2 - x1_2;
% then the area would be
% A4_2 = 1 / 2 * x2_2 * Y2_2, which is
A4_2 = 1 / 2 * x2_2 * (A1_2 - Mb_2);

% and for the third triangle,
% Let Y2_2 be the height and x3_2 be the base of the 2nd triangle where
% Y2_2 = A1_2 - Mb_2; (from eqA_2)
% and the base is
x3_2 = L_2 - La_2;
% then the area would be
% A5_2 = 1 / 2 * x3_2 * Y2_2, which is
A5_2 = 1 / 2 * (L_2 - La_2) * (A1_2 - Mb_2);

% We introduces as much equations as the number of variables and still need
% two more equations to solve the system.

% Lets apply the moment area method Theorem 2 for Point e w.r.t the base
% The deformation is
syms te_2; % or
% teb_2  = te_2 where
eq1_2 = te_2 == 1 / (E_2 * I_2) * ( -A3_2 * (x3_2 + x2_2 + 2/3 * x1_2) ...
                       +A4_2 * (x3_2 + 1/3 * x2_2) ...
                       +A5_2 * (2/3 * x3_2));
% and the deformation at Point a w.r.t the base is:
syms ta_2; % Which is also a known quantity
% tab_2 = ta_2
eq2_2 = ta_2 == 1 / (E_1 * I_2) * ( -A3_2 * (x2_2 + 2/3 * x1_2) ...
                       +A4_2 * (1/3 * x2_2));
                   
% eq2_2 brings in one more variable, so it doesnt count in the solution                  

% eq1_1, eq2_1, eq1_2, eq2_2 all have six unknowns yet. We need two more equaiton
% for this.
% eq1_1 => Pa_1, Pe_1, te_1
% eq2_1 => Pa_1, Pe_1,                          [ta_1]
% eq1_2 =>                  Pa_2, Pe_2, te_2
% eq2_2 =>                  Pa_2, Pe_2,         [ta_2]

% Also, ta_1 and ta_2 are being considered as constants. Since they
% depend upon each other, they will always appear together in equations
% once the variables have been substituted for with constants (including
% these 2)


% In case of mid support beam, we are swapping the point of application and
% effect with each other. We are not changing any equations and symbols.
% Just the meaning of ta_2. Pa_2, te_2, Pe_2 will be swapped together
% Lets bring two more equations then.
eq12_1 = te_1 == ta_2; % instead of te_1 == te_2;
eq12_2 = Pe_1 == Pa_2; % instead of Pe_1 == -Pe_2 because these two forces are in the same direction

% and lets try to solve the system

sol_b12 = solve([eq1_1, eq2_1, eq1_2, eq2_2, eq12_1, eq12_2], [Pa_1, Pe_1, te_1, Pa_2, Pe_2, ta_2]);
% instead of , [..., te_2], we write , [... , ta_2]
ta_2 = sol_b12.ta_2; % instead of te_2 = sol_b12.te_2; because we have effect on point a instead


%% Test the solution
% Lets assume constant, realistic values for the know values
if (1)
    L_1 = 50e-3;
    La_1 = 20e-3;
    E_1 = 200e9;
    b_1 = 5e-3;
    d_1 = 0.5e-3;
    lc_1 = 0.5 / 200/32;

    L_2 = 50e-3;
    La_2 = 40e-3;
    E_2 = 200e9;
    b_2 = 10e-3;
    d_2 = 5e-3;
    lc_2 = 0.5 / 200/32;
    te_2 = 0; % instead of ta_2
  
    ta_1 = lc_1;
    t1= vpa(subs(ta_2)); % instead of te_2
    fprintf('ta_1: %.10f\n', double(vpa(ta_1)));
    fprintf('ta_2: %.10f\n', double(vpa(te_2))); % instead of  ta_2 because we have effect on point a instead
    
    ta_1 = ta_1 + lc_1; % Give one step
    t2= vpa(subs(ta_2)); % instead of te_2
    tn = t2 - t1
    TF = tn / lc_1
    fprintf('Minimum nano movement: %.2f\n', double(vpa(tn)) * 1e9);

end










