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
package com.raytheon.uf.common.localization.backup.request;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.IRefreshableServerRequest;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to save a localization file. Includes the entire contents of the file
 * to save.
 *
 * NOTE: This is meant only for use with BackupService. If you need an API for
 * generic localization file operations then use the localization REST
 * interface.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2016  5937       tgurney     Initial creation
 * Oct  8, 2019 7929       tgurney     Implement refresh(). Add file checksum
 *
 * </pre>
 *
 * @author tgurney
 */

@DynamicSerialize
public class LocalizationFileSaveRequest implements IRefreshableServerRequest {
    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    @DynamicSerializeElement
    private byte[] bytes;

    @DynamicSerializeElement
    private String path;

    @DynamicSerializeElement
    private LocalizationContext context;

    @DynamicSerializeElement
    private String checksum;

    public LocalizationFileSaveRequest() {
    }

    public LocalizationFileSaveRequest(ILocalizationFile lf)
            throws IOException, LocalizationException {
        this.path = lf.getPath();
        this.context = lf.getContext();
        this.checksum = lf.getCheckSum();
        try (InputStream inStream = lf.openInputStream()) {
            this.bytes = IOUtils.toByteArray(inStream);
        }
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public LocalizationContext getContext() {
        return context;
    }

    public void setContext(LocalizationContext context) {
        this.context = context;
    }

    public byte[] getBytes() {
        return bytes;
    }

    public void setBytes(byte[] bytes) {
        this.bytes = bytes;
    }

    public String getChecksum() {
        return checksum;
    }

    public void setChecksum(String checksum) {
        this.checksum = checksum;
    }

    @Override
    public Optional<IServerRequest> refresh() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pathMgr.getLocalizationFile(context, path);
        if (lf.exists() && !lf.getCheckSum().equals(checksum)) {
            try {
                return Optional.of(new LocalizationFileSaveRequest(lf));
            } catch (IOException | LocalizationException e) {
                logger.warn("Failed to create a new save request for " + lf
                        + ". Returning the old request unchanged", e);
            }
        } else if (!lf.exists()) {
            LocalizationFileDeleteRequest newReq = new LocalizationFileDeleteRequest();
            newReq.setContext(context);
            newReq.setPath(path);
            return Optional.of(newReq);
        }
        return Optional.of(this);
    }
}
