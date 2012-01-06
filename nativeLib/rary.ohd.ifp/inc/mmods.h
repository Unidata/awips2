#include  <stdio.h>
/*  uhgFlag is used in event_loop, mods_plot and mp_done */
/* when UHG,RO, and RI is created set flag to true       */
/* In event_loop, check the flag,and destroy the mods_plot */
/* window when not using any more and set flag to 0    */
int uhgFlag;
