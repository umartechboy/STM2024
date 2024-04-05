clc; clear;
syms E I L L_a K2
syms x3 x4 x5 A1 A2 A3 A4 A5 P_a R_B M_B t_a P_e
syms t_e
%L = 2;
%L_a = 1;
%E = 1;
%I = 1;
%K2 = 1;

% Hooks law
%P_e = t_e * K2;
% sum of moments at B is zero
eq1 = M_B - P_a * L_a + P_e * L == 0;

eq2 = x3 / M_B == x4 / A2;
eq3 = x3 + x4 == L_a;
eq4 = x5 == L - L_a;

% For the SFD
%eq5 = A1 == R_B * L_a;
eq6 = A2 == P_e * (L - L_a);
eq7 = A3 == 1 / 2 * x3 * M_B;
eq8 = A4 == 1 / 2 * x4 * A2;
eq9 = A5 == 1 / 2 * x5 * A2;

eq10 = t_e == (...
    - A3 * (x5 + x4 + x3 * 2 / 3) ...
    + A4 * (x5 + x4 / 3) ...
    + A5 * (x5 * 2 / 3)) / (E * I);

eq11 = t_a == (...
    - A3 * (x4 + x3 * 2 / 3) ...
    + A4 * (x4 / 3)) / (E * I);

% sum of forces is zero
%eq12 = R_B - P_a + P_e == 0;


sol = solve([eq1, eq2, eq3, eq4, eq6, eq7, eq8, eq9, eq10, eq11], [x3, x4, x5, A1, A2, A3, A4, A5, M_B, t_e, P_e])

%solve(eq13, t_e)


%%
A1 = sol.A1
A2 = sol.A2
A3 = sol.A3
A4 = sol.A4
A5 = sol.A5


x3
x4
x5


% MAM - T2 on Point a
t_a = ( ...
    - A3 * (x4 + x3 * 2 / 3) ...
    + A4 * x4 / 3) / (E * I)

% MAM - T2 on Point e
eq10 = t_e == (...
    - A3 * (x5 + x4 + x3 * 2 / 3) ...
    + A4 * (x5 + x4 / 3) ...
    + A5 * (x5 * 2 / 3)) / (E * I);

% The spring constant
eq11 = t_e == P_e / K2;
eq1 = t_e == (...
    - A3 * (x5 + x4 + x3 * 2 / 3) ...
    + A4 * (x5 + x4 / 3) ...
    + A5 * (x5 * 2 / 3)) / (E * I);
