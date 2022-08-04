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
package com.raytheon.uf.common.dataplugin.gfe.textproduct;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Handles saving and loading of draft GFE text products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2010            randerso    Initial creation
 * Mar 26, 2014  #2884     randerso    Code clean up
 * Feb 24, 2015   3978     njensen     Changed to use abstract InputStream
 * Aug 18, 2015 3806       njensen     Use SaveableOutputStream to save
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class DraftProduct {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DraftProduct.class);

    @DynamicSerializeElement
    private ProductDefinition productDefinition;

    @DynamicSerializeElement
    private String productText;

    public DraftProduct() {

    }

    public DraftProduct(ProductDefinition productDefinition, String productText) {
        this.productDefinition = productDefinition;
        this.productText = productText;
    }

    public ProductDefinition getProductDefinition() {
        return productDefinition;
    }

    public void setProductDefinition(ProductDefinition productDefinition) {
        this.productDefinition = productDefinition;
    }

    public String getProductText() {
        return productText;
    }

    public void setProductText(String productText) {
        this.productText = productText;
    }

    public void save(LocalizationFile lf) throws SerializationException {
        File file = lf.getFile();
        file.getParentFile().mkdirs();
        byte[] bytes = SerializationUtil.transformToThrift(this);

        try (SaveableOutputStream sos = lf.openOutputStream()) {
            sos.write(bytes);
            sos.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public static DraftProduct load(LocalizationFile lf)
            throws SerializationException {

        byte[] bytes = null;
        InputStream in = null;
        try {
            File file = lf.getFile(true);
            in = lf.openInputStream();
            bytes = new byte[(int) file.length()];
            in.read(bytes);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

        return SerializationUtil.transformFromThrift(DraftProduct.class, bytes);
    }
}
