package com.raytheon.uf.edex.plugin.ffmp;

import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;

public class FFMPNestTest {

    public FFMPNestTest() {
        // for (int p = 1; p < 2; p++) {
        ArrayList<SourceBinEntry> nestPoints = processNest(1, 25, 25, 3, 3);

        // }
    }

    /**
     * Process gradually increasing nest looking for points that fall within the
     * geometry
     * 
     * @param geom
     * @param p
     * @param nx
     * @param ny
     * @param x
     * @param y
     * @param points
     * @return
     */
    private ArrayList<SourceBinEntry> processNest(int p, int nx, int ny, int x,
            int y) {
        ArrayList<SourceBinEntry> myPoints = new ArrayList<SourceBinEntry>();

        for (int i = p * (-1); i <= p; i++) {
            int xx = x + i;
            // process entire row
            if (i == p * (-1) || i == p) {
                for (int j = p * (-1); j <= p; j++) {
                    int yy = y + j;
                    System.out.println(" XX: " + xx + " YY: " + yy);
                }
            }
            // process only book ends
            else {
                for (int j = p * (-1); j <= p; j++) {
                    int yy = y + j;
                    if (yy == y - p || yy == y + p) {
                        System.out.println(" XX: " + xx + " YY: " + yy);
                    }
                }
            }

        }

        return myPoints;
    }

    public static void main(String[] args) {
        FFMPNestTest test = new FFMPNestTest();

    }
}
