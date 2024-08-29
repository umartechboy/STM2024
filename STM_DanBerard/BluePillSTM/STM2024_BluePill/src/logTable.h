/**************************************************************************/
/*

Daniel Berard, daniel.berard@mail.mcgill.ca
Last updated Oct 14, 2015
http://dberard.com/home-built-stm/


Table generated using the following MATLAB code:

    x=0:32768;
    lt = round(log(x+1)*(2^19-1)/log(2^15+1))';
    fid = fopen('lt.txt', 'a');
    fprintf(fid, [repmat('%d, ', 1, size(lt, 2) - 1), '%d,\r\n'], lt.');
    fclose(fid);


logTable takes an input from 0 - 32768 (the absolute value of the input 
from the 16-bit ADC) and outputs a value from 0 - 524287.

*/
/**************************************************************************/

extern const int logTable[];
