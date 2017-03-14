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
package com.raytheon.uf.viz.damagepath;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.MapUtils;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.damagepath.DamagePathHazardTypes.HazardTypeProperties;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.DateTimeEntry;

/**
 * Dialog to add/remove the key/value pairs that are part of the "properties"
 * member object in GeoJSON Feature objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2015  #4354     dgilling     Initial creation based on dialog from 
 *                                      lvenable.
 * Apr 30, 2015  #4354     dgilling     Allow edits to properties.
 * Jan 27, 2016  #5287     dgilling     Re-write based on CAVEJFACEDialog, use
 *                                      pre-defined properties/attributes.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class EditGeoJsonPropertiesDlg extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String HAZARD_TYPES_CONFIG_PATH = "damagepath"
            + IPathManager.SEPARATOR + "hazard-types.xml";

    private static final boolean SHOW_EVENT_ID = false;

    private static final int EVENTID_CHAR_LIMIT = 36;

    private static final int NAME_CHAR_LIMIT = 50;

    private static final int COMMENTS_CHAR_LIMIT = 200;

    private static final int SITEID_FIELD_WIDTH = 3;

    private static final int DEFAULT_FIELD_WIDTH = 20;

    private final Map<String, String> initialProperties;

    private final Map<String, String> newProperties;

    private Combo hazardType;

    private Text name;

    private Text comments;

    private DateTimeEntry eventTime;

    /**
     * Construct an instance of this dialog with the specified initial values.
     * 
     * @param parentShell
     *            the parent shell
     * @param initalProperties
     *            {@code Map} containing the initial properties and values for
     *            this damage path.
     */
    public EditGeoJsonPropertiesDlg(Shell parentShell,
            Map<String, String> initalProperties) {
        super(parentShell);
        setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE);
        this.initialProperties = new HashMap<>(initalProperties);
        this.newProperties = new HashMap<>();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("GeoJSON Properties Editor");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        composite.setLayout(new GridLayout(2, false));
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GC gc = new GC(getShell().getDisplay());
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int lineHeight = gc.getFontMetrics().getHeight();
        gc.dispose();

        Label eventIDLabel = new Label(composite, SWT.NONE);
        eventIDLabel.setText("Event ID:");
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        eventIDLabel.setLayoutData(gd);
        eventIDLabel.setVisible(SHOW_EVENT_ID);

        Text eventID = new Text(composite, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        eventID.setTextLimit(EVENTID_CHAR_LIMIT);
        String initialEventID = MapUtils.getString(initialProperties,
                DamagePathGeoJsonUtils.EVENTID_PROP_NAME,
                DamagePathGeoJsonUtils.generateEventID());
        eventID.setText(initialEventID);
        newProperties.put(DamagePathGeoJsonUtils.EVENTID_PROP_NAME,
                initialEventID);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = eventID.computeTrim(0, 0,
                (charWidth * EVENTID_CHAR_LIMIT), SWT.DEFAULT).width;
        eventID.setLayoutData(gd);
        eventID.setVisible(SHOW_EVENT_ID);

        Label hazardTypeLabel = new Label(composite, SWT.NONE);
        hazardTypeLabel.setText("Hazard Type:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        hazardTypeLabel.setLayoutData(gd);

        hazardType = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
        hazardType.setItems(getHazards());
        String initialHazard = initialProperties
                .get(DamagePathGeoJsonUtils.HAZARD_TYPE_PROP_NAME);
        if (initialHazard != null) {
            initialHazard = initialHazard.trim();
            int i = 0;
            for (String item : hazardType.getItems()) {
                if (HazardTypeProperties.getAbbreviationFromUIString(
                        item.trim()).equals(initialHazard)) {
                    hazardType.select(i);
                    break;
                }

                i++;
            }
        }
        hazardType.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                Combo comboWidget = (Combo) e.widget;
                getButton(IDialogConstants.OK_ID).setEnabled(
                        !comboWidget.getText().trim().isEmpty());
            }
        });

        Label nameLabel = new Label(composite, SWT.NONE);
        nameLabel.setText("Name:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        nameLabel.setLayoutData(gd);

        name = new Text(composite, SWT.SINGLE | SWT.BORDER);
        name.setTextLimit(NAME_CHAR_LIMIT);
        String initialName = initialProperties
                .get(DamagePathGeoJsonUtils.NAME_PROP_NAME);
        if (initialName != null) {
            name.setText(initialName.trim());
        }
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = name.computeTrim(0, 0, (charWidth * NAME_CHAR_LIMIT),
                SWT.DEFAULT).width;
        name.setLayoutData(gd);

        Label dateTimeLabel = new Label(composite, SWT.NONE);
        dateTimeLabel.setText("Event Time:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        dateTimeLabel.setLayoutData(gd);

        eventTime = new DateTimeEntry(composite);
        eventTime.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        Date initialEventTime = DamagePathGeoJsonUtils.getDefaultEventTime();
        String initialEventTimeString = initialProperties
                .get(DamagePathGeoJsonUtils.EVENT_TIME_PROP_NAME);
        if (initialEventTimeString != null) {
            try {
                initialEventTime = DamagePathGeoJsonUtils
                        .parseDateString(initialEventTimeString);
            } catch (ParseException e) {
                statusHandler.warn("Encountered invalid event time format: "
                        + initialEventTimeString);
            }
        }
        eventTime.setDate(initialEventTime);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        eventTime.setLayoutData(gd);

        Label commentsLabel = new Label(composite, SWT.NONE);
        commentsLabel.setText("Comments:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        commentsLabel.setLayoutData(gd);

        comments = new Text(composite, SWT.MULTI | SWT.V_SCROLL | SWT.WRAP
                | SWT.BORDER);
        comments.setTextLimit(COMMENTS_CHAR_LIMIT);
        String initialComments = initialProperties
                .get(DamagePathGeoJsonUtils.COMMENTS_PROP_NAME);
        if (initialComments != null) {
            comments.setText(initialComments.trim());
        }
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        int numLines = COMMENTS_CHAR_LIMIT / NAME_CHAR_LIMIT;
        Rectangle commentsTrimSize = comments.computeTrim(0, 0,
                (charWidth * NAME_CHAR_LIMIT), (lineHeight * numLines));
        gd.widthHint = commentsTrimSize.width;
        gd.heightHint = commentsTrimSize.height;
        comments.setLayoutData(gd);

        Label siteIdLabel = new Label(composite, SWT.NONE);
        siteIdLabel.setText("CWA:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        siteIdLabel.setLayoutData(gd);

        Text siteID = new Text(composite, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        String caveSiteID = DamagePathGeoJsonUtils.getDefaultSiteID();
        String initialSiteID = MapUtils.getString(initialProperties,
                DamagePathGeoJsonUtils.SITEID_PROP_NAME, caveSiteID);
        siteID.setText(initialSiteID);
        newProperties.put(DamagePathGeoJsonUtils.SITEID_PROP_NAME,
                initialSiteID);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = siteID.computeTrim(0, 0,
                (charWidth * SITEID_FIELD_WIDTH), SWT.DEFAULT).width;
        siteID.setLayoutData(gd);

        Label workstationLabel = new Label(composite, SWT.NONE);
        workstationLabel.setText("Workstation:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        workstationLabel.setLayoutData(gd);

        Text workstationID = new Text(composite, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        String caveWorkstationID = DamagePathGeoJsonUtils
                .getDefaultWorkstationID();
        String initialWorkstationID = MapUtils
                .getString(initialProperties,
                        DamagePathGeoJsonUtils.WORKSTATION_PROP_NAME,
                        caveWorkstationID);
        workstationID.setText(initialWorkstationID);
        newProperties.put(DamagePathGeoJsonUtils.WORKSTATION_PROP_NAME,
                initialWorkstationID);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = workstationID.computeTrim(0, 0,
                (charWidth * DEFAULT_FIELD_WIDTH), SWT.DEFAULT).width;
        workstationID.setLayoutData(gd);

        Label userLabel = new Label(composite, SWT.NONE);
        userLabel.setText("User:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, true);
        userLabel.setLayoutData(gd);

        Text userID = new Text(composite, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        String caveUserName = DamagePathGeoJsonUtils.getDefaultUser();
        String initialUserID = MapUtils.getString(initialProperties,
                DamagePathGeoJsonUtils.USER_PROP_NAME, caveUserName);
        userID.setText(initialUserID);
        newProperties.put(DamagePathGeoJsonUtils.USER_PROP_NAME, initialUserID);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.widthHint = userID.computeTrim(0, 0,
                (charWidth * DEFAULT_FIELD_WIDTH), SWT.DEFAULT).width;
        userID.setLayoutData(gd);

        return composite;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        Control buttonBar = super.createButtonBar(parent);
        getButton(IDialogConstants.OK_ID).setEnabled(
                !hazardType.getText().trim().isEmpty());
        return buttonBar;
    }

    private String[] getHazards() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile file = pathMgr.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, HAZARD_TYPES_CONFIG_PATH);

        String[] items;
        if (file != null) {
            try (InputStream inStream = file.openInputStream()) {
                SingleTypeJAXBManager<DamagePathHazardTypes> jaxb = SingleTypeJAXBManager
                        .createWithoutException(DamagePathHazardTypes.class);
                DamagePathHazardTypes hazTypes = jaxb
                        .unmarshalFromInputStream(inStream);
                Collection<String> itemsFromConfig = new ArrayList<>(hazTypes
                        .getHazards().size());
                for (HazardTypeProperties hazard : hazTypes.getHazards()) {
                    itemsFromConfig.add(hazard.toUIString());
                }
                items = itemsFromConfig.toArray(new String[0]);
            } catch (IOException | LocalizationException e) {
                String msg = String.format(
                        "Unable to open configuration file %s:",
                        HAZARD_TYPES_CONFIG_PATH);
                statusHandler.error(msg + e.getLocalizedMessage(), e);
                items = new String[0];
            } catch (Exception e) {
                String msg = String.format(
                        "Unable to read configuration file %s:",
                        HAZARD_TYPES_CONFIG_PATH);
                statusHandler.error(msg + e.getLocalizedMessage(), e);
                items = new String[0];
            }
        } else {
            String msg = String
                    .format("Unable to find configuration file %s in localization store.",
                            HAZARD_TYPES_CONFIG_PATH);
            statusHandler.warn(msg);
            items = new String[0];
        }

        return items;
    }

    @Override
    protected void setReturnCode(int code) {
        super.setReturnCode(code);

        if (code == OK) {
            newProperties.put(DamagePathGeoJsonUtils.HAZARD_TYPE_PROP_NAME,
                    HazardTypeProperties.getAbbreviationFromUIString(hazardType
                            .getText().trim()));
            newProperties.put(DamagePathGeoJsonUtils.NAME_PROP_NAME, name
                    .getText().trim());
            newProperties.put(DamagePathGeoJsonUtils.COMMENTS_PROP_NAME,
                    comments.getText().trim());
            newProperties
                    .put(DamagePathGeoJsonUtils.EVENT_TIME_PROP_NAME,
                            DamagePathGeoJsonUtils.formatEventTime(eventTime
                                    .getDate()));
        }
    }

    /**
     * Returns the user-entered properties for this damage path.
     * 
     * @return {@code Map} containing the properties for this damage path. Will
     *         be empty if user clicked Cancel.
     */
    public Map<String, String> getProperties() {
        return newProperties;
    }
}