/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4.cf;

import java.util.regex.Pattern;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CfUtils {

    public static final Pattern CF_NAME_PATTERN = Pattern
            .compile("^[a-zA-Z][a-zA-Z0-9_]*$");

    public static boolean validName(String name) {
        return CF_NAME_PATTERN.matcher(name).matches();
    }
}
