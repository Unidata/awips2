/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.mpe.util;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_6hr;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            snaples     Initial creation
 * Jul 16, 2015 17561      snaples     Updated to fix issues with QPE Grid.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class DisaggGridMethod {

    Station[] disagg_station_6hr = Disagg6Hr.disagg_station_6hr;

    Values_1hr[] disaggValues = Disagg6Hr.disaggValues;

    double[][][] QPEgrids = Disagg6Hr.QPEgrids;

    int val6hreq0 = Disagg6Hr.val6hreq0;

    int val6hrgt0 = Disagg6Hr.val6hrgt0;

    int disagg_db_factor = Disagg6Hr.disagg_db_factor;

    Values_6hr[] values6hr = Disagg6Hr.values6hr;

    BufferedWriter disagg_log_fd = Disagg6Hr.disagg_log_fd;

    String[] obsdate = Disagg6Hr.obsdate;

    Date[] obsdate_date_t = Disagg6Hr.obsdate_date_t;

    double[] total_6hr;

    double[][] totals_1hr;

    public void grid_cleanup() {

        total_6hr = null;
        totals_1hr = null;

    }

    public void disaggGridMethod() {

        DailyQcUtils dqc = DailyQcUtils.getInstance();
        int i, j, k, l, index, nn, mm;
        int hour, hrapx, hrapy, num_miss;
        int hxmm, hynn, nbox;
        double total, diff_1hr;
        Calendar nt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date end_time_temp;
        Date endtime_disagg = Disagg6Hr.endtime_disagg;
        int num_disagg_stations = Disagg6Hr.num_disagg_stations;
        int num_days_to_qc = dqc.qcDays;
        BufferedWriter out = disagg_log_fd;

        /*--------------------------------------*/
        /* initialize other variables */
        /* double totals_1hr[6][num_disagg_stations] */
        /* double total_6hr[num_disagg_stations] */
        /*--------------------------------------*/
        total_6hr = new double[num_disagg_stations];
        totals_1hr = new double[6][num_disagg_stations];

        end_time_temp = endtime_disagg;

        try {

            for (j = 0; j < num_days_to_qc; j++)
            {
                for (k = 0; k < 4; k++)// loop on 4 6hr periods
                {
                 
                    out.write(String.format("\n   -- 6hr Period %d -- \n", k));
                    out.flush();
                    
                    /*---------------------------------------------------*/
                    /* read QPE grids from files into QPEgrids array     */
                    /* if QPE grids are missing then num_miss set to 99  */
                    /*---------------------------------------------------*/
                    num_miss = getQPEGrids(j, k);

                    if (num_miss > 0)
                    {
                        /*---------------------------------------------------------*/
                        /* case of missing QPE grid(s) */
                        /* set all 1hr values to missing for all disagg stations */
                        /* and take next 6hr period */
                        /*---------------------------------------------------------*/

                        out.write(String.format("QPE grids missing -- all 1hr values set to missing\n"));
                        out.flush();

                        for (l = 0; l < 6; l++)
                        {
                            for (i = 0; i < num_disagg_stations; i++)
                            {
                                disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = -9999;

                            }

                        }
                        continue;

                    }
                    else
                    {

                        /*------------------------------------------------------------*/
                        /* case of all 6 QPE grids available */
                        /*                                                            */
                        /* calculate average of 9 nearest neighbor grid values */
                        /* to location of gage */
                        /*------------------------------------------------------------*/
                        /* QPEgrids[l][i][j] array */
                        /* l = 0,1,2,3,4,5 */
                        /* i = num of cols */
                        /* j = num of rows */
                        /* values6hr[].hrapx_local = HRAP x coord of station */
                        /* values6hr[].hrapy_local = HRAP y coord of station */
                        /*                                                            */
                        /* nbox = number of boxes sampled */
                        /* - can be less than 9 if near the edge of the area */
                        /*                                                            */
                        /* tests for hxmm > maxx, hxmm < 0, etc check for bins */
                        /* beyond the bounds of the area (array) */
                        /*------------------------------------------------------------*/

                        for (i = 0; i < num_disagg_stations; i++)
                        {
                            index = j * num_disagg_stations + i;

                            if (values6hr[index].ID.equals(Disagg6Hr.DISAGG_MISSING_STATION_SYMBOL))
                            {
                                continue;
                            }

                            hrapx = values6hr[index].hrapx_local;
                            hrapy = values6hr[index].hrapy_local;
                            
                            total_6hr[i] = 0.0;

                            for (l = 0; l < 6; l++)
                            {

                                total = 0;
                                nbox = 0;

                                for (nn = -1; nn < 2; nn++)
                                {
                                    hynn = hrapy + nn;
                                    if (hynn >= Disagg6Hr.disagg_maxy)
                                    {
                                        continue;
                                    }
                                    
                                    if (hynn < 0)
                                    {
                                        continue;
                                    }

                                    for (mm = -1; mm < 2; mm++)
                                    {
                                        hxmm = hrapx + mm;
                                        if (hxmm >= Disagg6Hr.disagg_maxx)
                                        {
                                            continue;
                                        }
                                        
                                        if (hxmm < 0)
                                        {
                                            continue;
                                        }

                                        nbox++;
                                        
                                        out.write(String.format("QPEgrids[%d][%d][%d] = %5.2f \n",
                                        		l, hynn, hxmm, 
                                        		QPEgrids[l][hynn][hxmm]));
                                        
                                        total = total + QPEgrids[l][hynn][hxmm];
                                    }
                                }

                                if (nbox > 0)
                                {
                                    totals_1hr[l][i] = total / 9.;
                                    total_6hr[i] = total_6hr[i] + total / 9.;
                                    out.write(String.format(" i=%d  l=%d  avg1hr=%5.2f\n", i, l, totals_1hr[l][i]));
                                }
                                else
                                {
                                    totals_1hr[l][i] = -9999.;
                                    total_6hr[i] = -9999.;
                                    out.write(String.format(" i=%d  l=%d  avg1hr = missing\n", i, l));
                                }
                            }

                        }

                        /*----------------------------------------------------------*/
                        /* if 6hr value is missing, */
                        /* then set all resulting 1hr values to missing */
                        /*
                         * check for case of 6hr value = 0.0, sum 1hr values >
                         * 0.0
                         */
                        /* val6hreq0 = 1,2 */
                        /* 1 -- ignore 1hr estimates, set all 1hr values = 0.0 */
                        /* 2 -- ignore 6hr value, use 1hr values from grids */
                        /*
                         * check for case of 6hr value > 0.0, sum 1hr values =
                         * 0.0
                         */
                        /* val6hrgt0 = 1,2 */
                        /* 1 -- ignore 6hr gage value, set all 1hr values to 0.0 */
                        /* 2 -- ignore 1hr estimated values, reestimate based on */
                        /* nearest neighbor 1hr gages */
                        /* (future enhancement) */
                        /*----------------------------------------------------------*/

                        for (i = 0; i < num_disagg_stations; i++)
                        {
                            index = j * num_disagg_stations + i;

                            if (values6hr[index].ID.equals(Disagg6Hr.DISAGG_MISSING_STATION_SYMBOL))
                            {
                                continue;
                            }

                            out.write(String.format("%s \n", disagg_station_6hr[i].hb5));

                            if (values6hr[index].value[k] == -9999.)
                            {
                                for (l = 0; l < 6; l++)
                                {
                                    disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = -9999.f;

                                }

                            }
                            else if (values6hr[index].value[k] == 0.0 && total_6hr[i] > 0.0)
                            {

                                if (val6hreq0 == 1)
                                {
                                    out.write("case of 6hr value = 0.0, sum 1hr values > 0.0 -- ");
                                    out.write("ignore 1hr estimates, set all 1hr values = 0.0\n");
                                    
                                    for (l = 0; l < 6; l++)
                                    {
                                        disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = 0.0f;

                                    }

                                }
                                else
                                {
                                    out.write("case of 6hr value = 0.0, sum 1hr values > 0.0 -- ");
                                    out.write("ignore 6hr value, use 1hr values from grids\n");
                                    for (l = 0; l < 6; l++)
                                    {
                                        disaggValues[j * num_disagg_stations
                                                + i].HourlyValues[6 * k + l] = (float) (totals_1hr[l][i] * disagg_db_factor);

                                    }

                                }

                            } 
                            else if (values6hr[index].value[k] > 0.0 && total_6hr[i] == 0.0)
                            {
                                if (val6hrgt0 == 1)
                                {
                                    out.write("case of 6hr value > 0.0, sum 1hr values = 0.0 -- ");
                                    out.write("ignore 6hr gage value, set all 1hr values to 0.0\n");
                                
                                    for (l = 0; l < 6; l++)
                                    {
                                        disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = 0.0f;

                                    }

                                }
                                else
                                {
                                    /*
                                     * future enhancement - use nearest neighbor
                                     * method to estimate 1hr values
                                     */

                                    out.write("case of 6hr value > 0.0, sum 1hr values = 0.0 -- ");
                                    out.write("ignore 6hr gage value, set all 1hr values to 0.0\n");
                                
                                    for (l = 0; l < 6; l++)
                                    {
                                        disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = 0.0f;
                                    }
                                }
                            } 
                            else
                            {
                                /*-------------------------------------------------*/
                                /*
                                 * compare 6hr value against sum of six 1hr
                                 * values
                                 */
                                /*
                                 * apply difference equally to all hours in
                                 * period
                                 */
                                /*-------------------------------------------------*/

                                diff_1hr = (values6hr[index].value[k] - total_6hr[i]) / 6.;

                                for (l = 0; l < 6; l++)
                                {
                                    disaggValues[j * num_disagg_stations + i].HourlyValues[6
                                            * k + l] = (float) ((totals_1hr[l][i] + diff_1hr) * disagg_db_factor);
                                    if (disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] < 0.0)
                                    {
                                        disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l] = 0.0f;
                                    }
                                }
                            }

                            for (l = 0; l < 6; l++)
                            {
                                hour = (k * 6) + l;
                                out.write(String.format("    %d    %6.1f\n",
                                        hour, disaggValues[j * num_disagg_stations + i].HourlyValues[6 * k + l]));
                                out.flush();
                            }

                        } /* end for(i=0;i<num_disagg_stations;i++) */

                    } /* end if(num_miss > 0) ... */

                } /* end loop on for(k=0;k<4;k++) */

                /*-----------------------------*/
                /* take next day */
                /*-----------------------------*/

                nt.setTime(end_time_temp);
                nt.add(Calendar.SECOND, 86400);
                end_time_temp = nt.getTime();

            } /* end loop on for(j=0;j<num_days_to_qc;j++) */

        } catch (IOException e) {
            return;
        }
    }

    public int getQPEGrids(int j, int k)
    {

        /*------------------------------------------------*/
        /* function to read QPE grids for a 6hr period */
        /* Input: */
        /* j = day number */
        /* k = period number */
        /* obsdate array - contains dates in yyyy-mm-dd format */
        /* disagg_maxx,disagg_maxy = max hrap x/y coord */
        /* qpe_files_dir = dir containing QPE grids */
        /* date_form = value of st3_date_form token */
        /* - format of date in filename */
        /*                                                */
        /* Output: */
        /* 1) QPE grid values stored in QPEgrids array    */
        /* 2) num_miss = number of missing QPE grids      */
        /*------------------------------------------------*/

        int i, ii, l, jj, hour, num_miss;
        String qpe_filename = "";
        double factor = 2540.;
        String qpe_files_dir = Disagg6Hr.qpe_files_dir;
        String date_form = Disagg6Hr.date_form;
        int disagg_maxx = Disagg6Hr.disagg_maxx;
        int disagg_maxy = Disagg6Hr.disagg_maxy;
        BufferedWriter out = disagg_log_fd;
        BufferedReader in = null;

        num_miss = 0;
        jj = j + 1;

        /*----------------------------------------------*/
        /* loop on 6 hours in each period */
        /*----------------------------------------------*/
        try {

            for (l = 0; l < 6; l++)
            {
                /*----------------------------------------------*/
                /* create file name */
                /* filenames are of the form: */
                /* xmrgmmddyyyyhhz for st3_date_form = mdY */
                /* xmrgyyyymmddhhz for st3_date_form = Ymd */
                /*----------------------------------------------*/

                hour = 12 + (k * 6) + l + 1;
                if (hour == 24)
                {
                    hour = 0;
                    jj = j;

                } else if (hour > 23)
                {
                    hour = ((k - 2) * 6) + l + 1;
                    jj = j;

                }

                if (date_form.charAt(0) == 'm')
                {
                    qpe_filename = String.format(
                            "%s/xmrg%c%c%c%c%c%c%c%c%02dz", qpe_files_dir,
                            obsdate[jj].charAt(5), obsdate[jj].charAt(6),
                            obsdate[jj].charAt(8), obsdate[jj].charAt(9),
                            obsdate[jj].charAt(0), obsdate[jj].charAt(1),
                            obsdate[jj].charAt(2), obsdate[jj].charAt(3), hour);

                }
                else
                {
                    qpe_filename = String.format(
                            "%s/xmrg%c%c%c%c%c%c%c%c%02dz", qpe_files_dir,
                            obsdate[jj].charAt(0), obsdate[jj].charAt(1),
                            obsdate[jj].charAt(2), obsdate[jj].charAt(3),
                            obsdate[jj].charAt(5), obsdate[jj].charAt(6),
                            obsdate[jj].charAt(8), obsdate[jj].charAt(9), hour);

                }

                /*----------------------------*/
                /* attempt to open the file */
                /*----------------------------*/

                out.write(String.format("attempting to open file %s\n", qpe_filename));
                out.flush();
                
                /*--------------------------------------*/
                /* read QPE file */
                /* format of file is xmrg */
                /* store values in QPEgrids array */
                /*--------------------------------------*/

                XmrgFile xmrgFile = new XmrgFile(qpe_filename);
                
                if ( xmrgFile.getFile().length() > 0)
    		    {
    		        xmrgFile.load();

    		        Rectangle hrapExtent = xmrgFile.getHrapExtent();

    		        short[][]  hourlyShortData = xmrgFile.getData(hrapExtent);


    		        int maxCols = hrapExtent.width;
    		        int maxRows = hrapExtent.height;

    		        double value = 0.0;

    		        for (int row = 0; row < maxRows; row++)
    		        {
    		            int rowValue = maxRows - row - 1;
    		            
    		        	for (int col = 0; col < maxCols; col++)
    		            {

    		                short shortValue = hourlyShortData[row][col];

    		                if (shortValue >= 0)
    		                {
    		                    //convert from hundredths of MM to inches
    		                    value = ( ((double) shortValue) / ( 25.4 * 100.0) );
    		             
    		                }
    		                else //keep special MISSING value
    		                {
    		                    value = shortValue;
    		                }

    		                QPEgrids[l][rowValue][col] = value;
    		                
    		                //if (value > 0.99)
    		                //{
    		                //    out.write(String.format(" in getQPEGrids -- QPEgrids[l][%d][%d] = %5.2f \n", rowValue, col, QPEgrids[l][rowValue][col]));
    		                //}
    		          
    		            } //end for col
    		        } //end for row

    		    }//end if length > 0
                else
                {
                    out.write("   error reading file\n" + qpe_filename + "\n" );
                    num_miss++;
                }
              
         
              
                
              
        
            
            } // end for (l = 0; l < 6; l++)

          
            return num_miss;
        } //end try
        catch (IOException e)
        {
            return 99;

        }
       
    }
}
