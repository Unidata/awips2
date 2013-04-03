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
package com.raytheon.uf.common.util.algorithm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Test;

import com.raytheon.uf.common.util.algorithm.AlgorithmUtil.IBinarySearchResponse;

/**
 * Test {@link AlgorithmUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2012            djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AlgorithmUtilTest {

    private static final char[] CHARS = new char[] { 'a', 'b', 'c', 'd', 'e',
            'f', 'g', 'h', 'i', 'j', 'k' };

    private static IBinarySearchResponse<Character> binarySearch(char[] chars,
            final char item) {
        SortedSet<Character> set = new TreeSet<Character>();
        for (char itemToAdd : chars) {
            set.add(Character.valueOf(itemToAdd));
        }
        return AlgorithmUtil.binarySearch(set, new Comparable<Character>() {
            @Override
            public int compareTo(Character o) {
                return o.compareTo(item);
            }
        });
    }

    @Test
    public void binarySearchReturnsCorrectValue() {
        assertEquals("Did not find the correct location of the correct value!",
                Character.valueOf('e'), binarySearch(CHARS, 'e').getItem());
    }

    @Test
    public void binarySearchFindsMidValueInFirstIteration() {
        assertEquals("Did not find the value in the correct iteration!", 1,
                binarySearch(CHARS, 'f').getIterations());
    }

    @Test
    public void binarySearchFindsEndValueInThreeIterations() {
        assertEquals("Did not find the value in the correct iteration!", 3,
                binarySearch(CHARS, 'j').getIterations());
    }

    @Test
    public void binarySearchFindsFirstValueInThreeIterations() {
        assertEquals("Did not find the value in the correct iteration!", 3,
                binarySearch(CHARS, 'a').getIterations());
    }

    @Test
    public void binarySearchFindsSecondValueInFourIterations() {
        assertEquals("Did not find the value in the correct iteration!", 4,
                binarySearch(CHARS, 'b').getIterations());
    }

    @Test
    public void binarySearchReturnsNullItemWhenNotInSet() {
        assertNull("Expected null item in the result", binarySearch(CHARS, 'z')
                .getItem());
    }
}
