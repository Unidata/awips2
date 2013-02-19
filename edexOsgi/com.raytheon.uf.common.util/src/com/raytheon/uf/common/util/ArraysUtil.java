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
package com.raytheon.uf.common.util;

/**
 * Array operation utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2013 1638       mschenke    Functions moved from edex.common Util
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ArraysUtil {

    /**
     * Flip double array along a horizontal line.
     * 
     * @param baseArray
     * @return
     */
    public static double[][] flipHoriz(double[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[y - i - 1][j] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Flip double array along a vertical line
     * 
     * @param baseArray
     * @return
     */
    public static double[][] flipVert(double[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[i][x - j - 1] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Flip float array along a horizontal line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(float[][] baseArray) {

        float temp;
        int height = baseArray[0].length;
        int width = baseArray.length;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i][j];
                baseArray[i][j] = baseArray[i][width - j - 1];
                baseArray[i][width - j - 1] = temp;
            }
        }
    }

    /**
     * Rotate 180 degrees.
     * 
     * @param baseArray
     * @return
     */
    public static void rotate180(float[] baseArray, int height, int width) {

        int counter = 0;
        int total = height * width;
        float temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[counter];
                baseArray[counter] = baseArray[total - counter - 1];
                baseArray[total - counter - 1] = temp;
                counter++;
            }
        }
    }

    /**
     * Flip along a vertical
     * 
     * @param baseArray
     * @return
     */
    public static void flipVert(float[] baseArray, int height, int width) {

        float temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i * width + j];
                baseArray[i * width + j] = baseArray[i * width + width - j - 1];
                baseArray[i * width + width - j - 1] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(float[] baseArray, int height, int width) {

        float temp = 0;

        for (int i = 0; i < height / 2; i++) {
            for (int j = 0; j < width; j++) {
                temp = baseArray[j + width * i];
                baseArray[j + width * i] = baseArray[j + width
                        * (height - i - 1)];
                baseArray[j + width * (height - i - 1)] = temp;
            }
        }
    }

    /**
     * Flip along a vertical
     * 
     * @param baseArray
     * @return
     */
    public static void flipVert(byte[] baseArray, int height, int width) {

        byte temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i * width + j];
                baseArray[i * width + j] = baseArray[i * width + width - j - 1];
                baseArray[i * width + width - j - 1] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(byte[] baseArray, int height, int width) {

        byte temp = 0;

        for (int i = 0; i < height / 2; i++) {
            for (int j = 0; j < width; j++) {
                temp = baseArray[j + width * i];
                baseArray[j + width * i] = baseArray[j + width
                        * (height - i - 1)];
                baseArray[j + width * (height - i - 1)] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line
     * 
     * @param baseArray
     * @return
     */
    public static float[][] flipVert(float[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        float[][] returnVal = new float[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[i][x - j - 1] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Rotate double array 90 degrees to the right
     * 
     * @param baseArray
     * @return
     */
    public static double[][] rotateRight(double[][] baseArray) {

        int y = baseArray[0].length;
        int x = baseArray.length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                returnVal[j][x - 1 - i] = baseArray[i][j];
            }
        }
        return returnVal;

    }

    /**
     * Rotate a double array 90 degrees to the left
     * 
     * @param baseArray
     * @return
     */
    public static double[][] rotateLeft(double[][] baseArray) {

        int y = baseArray[0].length;
        int x = baseArray.length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                returnVal[y - j - 1][i] = baseArray[i][j];
            }
        }
        return returnVal;

    }

}
