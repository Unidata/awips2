package com.raytheon.viz.hydrobase.dialogs;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * This class is a factory for Areal Definitions Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 * Oct 22, 2020 22303      mgamazaychikov Changed how geoType is defined
 * </pre>
 *
 * @author mgamazaychikov
 *
 */

public class ArealDefinitionsDlgFactory {

    private static final String WHFS_GEODATA_TYPE = "whfs_geodata_type";

    private static final String TYPE_SHP = "SHP";

    public static AbstractArealDefinitionsDlg createArealDefinitionsDlg(Shell shell) {
        String geoType = AppsDefaults.getInstance().getToken(WHFS_GEODATA_TYPE);
        if (geoType == null || !geoType.equals(TYPE_SHP)) {
            return new ArealDefinitionsDlgASCII(shell);
        } else {
            return new ArealDefinitionsDlgSHP(shell);
        }
    }

}
