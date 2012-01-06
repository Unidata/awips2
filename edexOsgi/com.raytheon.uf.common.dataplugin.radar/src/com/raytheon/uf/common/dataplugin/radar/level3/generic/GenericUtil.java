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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent.ComponentType;

/**
 * Contains utility methods to handle the data structures in the Generic Data
 * type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class GenericUtil {
    public static List<GenericDataParameter> parseParameters(DataInputStream in)
            throws IOException {
        List<GenericDataParameter> parameters = new ArrayList<GenericDataParameter>();

        // There is supposed to be a pointer prior to the data
        // Doesn't seem to really matter?
        // handlePointer(in);

        // Read in the number of parameters
        int numParams = in.readInt();
        if (numParams > 0) {
            // redundant
            numParams = in.readInt();
        }

        // Read in each Parameter
        GenericDataParameter currParameter;
        for (int i = 0; i < numParams; i++) {
            currParameter = new GenericDataParameter();

            // Get the id
            currParameter.setId(readInString(in));

            // Get the attributes for the parameter
            currParameter.addAttributePairs(readInString(in));

            parameters.add(currParameter);
        }

        return parameters;
    }

    public static List<GenericDataComponent> parseComponents(DataInputStream in)
            throws IOException {
        List<GenericDataComponent> components = new ArrayList<GenericDataComponent>();
        // There is supposed to be a pointer prior to the data
        // Doesn't seem to really matter?
        // handlePointer(in, 1);

        // Read in the number of parameters
        int numComp = in.readInt();

        if (numComp != 0) {
            // redundant
            numComp = in.readInt();
        }

        // Read in each Parameter
        GenericDataComponent currComponent;
        ComponentType componentType;
        for (int i = 0; i < numComp; i++) {
            // I think this should be 1
            in.readInt();

            componentType = ComponentType.valueOf(in.readInt());
            currComponent = componentType.getNewInstance();

            // Handle the component specify data
            currComponent.parseData(in);

            components.add(currComponent);
        }

        return components;
    }

    public static void handlePointer(DataInputStream in) throws IOException {
        handlePointer(in, 50);
    }

    /**
     * 
     * @param in
     * @throws IOException
     */
    public static void handlePointer(DataInputStream in, int maxBytes)
            throws IOException {
        // Read in bytes until the byte != 0
        // Not sure what this number is, since it is not in the ICD. It seems to
        // be redundant or some kind of pointer.

        int bytesRead = 0;
        byte currentByte;
        do {
            currentByte = in.readByte();
            bytesRead++;
        } while (currentByte == 0 && bytesRead < maxBytes);
    }

    /**
     * 
     * @param in
     * @return
     * @throws IOException
     */
    public static String readInString(DataInputStream in) throws IOException {
        StringBuffer rval = new StringBuffer();
        char currentChar;
        int length;

        // This is a cheap work around, since the String data type specified in
        // the ICD doesn't match what is actually in the packet.
        // Needless to say, it might break if the String is not present.

        // Read until the byte is not 0
        // currentChar = (char) (in.readByte() & 0xFF);
        // while (currentChar == '\0') {
        // currentChar = (char) (in.readByte() & 0xFF);
        // }

        // The first byte that is not 0 is the number of characters in the
        // String.
        length = in.readInt();

        // Read in the number of characters
        for (int i = 0; i < length; i++) {
            currentChar = (char) (in.readByte() & 0xFF);
            rval.append(currentChar);
        }

        // Handle the XDR padding since XDR pads Strings with 0's until the
        // length is divisible by 4
        int paddingSize = length % 4;

        if (paddingSize != 0) {
            paddingSize = 4 - paddingSize;
        }

        for (int i = 0; i < paddingSize; i++) {
            in.readByte();
        }

        // Return the full String
        return rval.toString();
    }

    /**
     * Finds the product code in the Generic Packet Product Description.
     * 
     * @param in
     * @return The product code for the Generic Packet.
     * @throws IOException
     */
    public static int getProductID(DataInputStream in) throws IOException {
        int productID = 0;

        in.mark(Integer.MAX_VALUE);

        in.skipBytes(6);

        // Read the Name
        GenericUtil.readInString(in);

        // Read the Description
        GenericUtil.readInString(in);

        productID = in.readInt();

        in.reset();

        return productID;
    }
}
