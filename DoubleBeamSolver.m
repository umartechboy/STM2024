clc; clear;
syms E1 I1 L1 L_a K2 L_f
syms x3 x4 x5 A1 A2 A3 A4 A5 P_a R_B M_B t_a P_e L2 E2 I2
syms t_e
% sum of moments at B is zero
eq1 = M_B - P_a * L_a + P_e * L1 == 0;

eq2 = x3 / M_B == x4 / A2;
eq3 = x3 + x4 == L_a;
eq4 = x5 == L1 - L_a;

% For the SFD
%eq5 = A1 == R_B * L_a;
eq6 = A2 == P_e * (L1 - L_a);
eq7 = A3 == 1 / 2 * x3 * M_B;
eq8 = A4 == 1 / 2 * x4 * A2;
eq9 = A5 == 1 / 2 * x5 * A2;

% MAM - T2 on Point e
eq10 = t_e == (...
    - A3 * (x5 + x4 + x3 * 2 / 3) ...
    + A4 * (x5 + x4 / 3) ...
    + A5 * (x5 * 2 / 3)) / (E1 * I1);

% MAM - T2 on Point a
eq11 = t_a == (...
    - A3 * (x4 + x3 * 2 / 3) ...
    + A4 * (x4 / 3)) / (E1 * I1);

% sum of forces is zero
%eq12 = R_B - P_a + P_e == 0;


sol = solve([eq1, eq2, eq3, eq4, eq6, eq7, eq8, eq9, eq10, eq11], [x3, x4, x5, A2, A3, A4, A5, M_B, P_a, P_e]);

eqA = P_e == sol.P_e;
%solve(eq13, t_e)


%%
% A1 = sol.A1
% A2 = sol.A2;
% A3 = sol.A3;
% A4 = sol.A4;
% A5 = sol.A5;


% The spring constant
% Hooks law
K2 = 1 / L2^3 * 3 * E2 * I2;
eqB = t_e == P_e / K2;
% t_e == P_E * L2^3 / 3 / E1 / I1
sol2 = solve([eqA, eqB], [t_e, P_e]);
t_e = sol2.t_e
P_e = sol2.P_e
%t_a = subs(sol.t_a, [P_e, t_e], [sol2.P_e, sol2.t_e])


%% Lets solve beam 2
% Test Beam 2 without beam 1
% syms P_e
% Sanity check
% L_f = 1/2*L2;
A8 = P_e * L2 * L_f;
A7 = 1 / 2 * (P_e * L2 * L_f / L2) * L_f;
A6 = A8 - 2 * A7;
t_n = ( ...
    (A6 * 0.5 * L_f) + ...
    (A7 * 2/3 * L_f)) / (E2 * I2)
    
%% t_n (nano diflaction) is a function of t_a (Applied diflaction). Can be verified by putting whole numbers in the constant values.
% subs(t_n, [L1, L2, E2, I2, L_a, E1, I1, K2, L2, L_f], [1, 2, 3, 4, 5, 6, 7, 8, 9, 0.5])

% Lets Make the calculator now.
