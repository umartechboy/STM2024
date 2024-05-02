clc; clear;

% From the Bending Moment Diagram of beam 1
% Let 
syms Mb_1; % be the moment at point B
syms Pa_1; % be the force at point of application,
syms Pe_1; % be the force at the end beam
syms ta_1; % be the applied deformation
syms te_1; % be the deformation at the end beam
syms La_1; % be the length at the point of application
syms L_1; % be the length of the beam
% 
% Sum of moments at B
eq1 = Mb_1 - Pa_1 * La_1 + Pe_1 * L_1 == 0; % Info. +4 equations needs

% The rectangles have these areas
A1_1 = La_1 * (Pa_1 - Pe_1); % +4
A2_1 = (La_1 - La_1) * Pe_1; % +4

% Lets Solve the tringles for Moment area method
% Let
syms Y1_1 x1_1 A3_1; % be the height and base of the first triangle with area
% A3_1 = 1 / 2 * 

%Let
syms Y2_1; % be the height of the triangle with Area
syms A4_1;
%Let
%syms Y2_1 be the height of the triangle with Area
syms A5_1;

% The areas would be
A3 = 1 / 2 * x1_1 * 

Y1_1 = Mb;
Y3_1 = A1_1;
Y2_1 = Y3_1 - Y1_1;
% from the geometry
% Y3_1 = Y2_1 + Y1_1
% Where Y1_1 = Mb and Y3_1 = Mb, this gives
% Y3_1 = Y1_1 + Y2_1
%eq2 = Y3_1 == Mb + Y2_1;
%eq3 = Y3_1 == La_1 * Mb;
