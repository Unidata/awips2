package com.raytheon.viz.mpe.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.common.CommandException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.ui.actions.SetDisplayField;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Abstract dynamic fields populator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2011  ?         rgeorge     Initial creation
 * Jan 25, 2017  19504     snaples     Fixed issue with Save Level 2 not being
 *                                     active during Daily QC.
 * Feb 28, 2017   6152     bkowal      Extracted {@link #menuItemEnabled(List, DisplayFieldData)} to
 *                                     allow for customized determinations of whether a menu item
 *                                     should be enabled.
 * Sep 21, 2017   6407     bkowal      Added override to ensure that the GOES-R SATPRE menu item would always
 *                                     be enabled when present.
 * May 10, 2018   7131     mduff       Changes for DQC dialog changes.
 * 
 * </pre>
 * 
 * @author rgeorge
 */
public abstract class FieldsPopulator extends CompoundContributionItem {
    private static final String COMMAND_ID = "com.raytheon.viz.mpe.ui.actions.savelevel2";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FieldsPopulator.class);

    protected static class MenuData {
        protected String text;

        protected String mnemonic;

        protected int type;

        protected String handler;

        protected Map<?, ?> params;

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         */
        public MenuData(String text, String mnemonic) {
            this(text, mnemonic, Action.AS_PUSH_BUTTON);
        }

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         * @param type
         *            The Action type like &lt;Action.AS_PUSH_BUTTON&gt;
         */
        public MenuData(String text, String mnemonic, int type) {
            this(text, mnemonic, type, null, null);
        }

        /**
         * @param text
         *            Button text
         * @param mnemonic
         *            The underlined character
         * @param type
         *            The Action type like &lt;Action.AS_PUSH_BUTTON&gt;
         * @param handler
         *            The handler id defined in plugin.xml
         * @param params
         *            The parameters of the handler
         */
        public MenuData(String text, String mnemonic, int type, String handler,
                Map<?, ?> params) {
            this.text = text;
            this.mnemonic = mnemonic;
            this.type = type;
            this.handler = handler;
            this.params = params;

        }
    }

    @Override
    protected IContributionItem[] getContributionItems() {
        AppsDefaults defaults = AppsDefaults.getInstance();
        String list = defaults.getToken("mpe_generate_list");
        List<String> fields = new ArrayList<>();
        if (list != null) {
            fields = new ArrayList<>(Arrays.asList(list.split("[,]")));
            Collections.sort(fields);
        }
        for (DisplayFieldData data : getMenuItems()) {
            boolean enabled = menuItemEnabled(fields, data);
            if (data == DisplayFieldData.goesRSatPre) {
                enabled = true;
            }
            ActionContributionItem itemFound = (ActionContributionItem) getMenuManger()
                    .find(getTexMap().get(data).handler);
            if (itemFound == null) {
                itemFound = (ActionContributionItem) getMenuManger()
                        .find(getTexMap().get(data).text);
            }
            if (itemFound == null) {
                getMenuManger().add(new ActionContributionItem(
                        new DisplayFieldAction(data, enabled)));
            } else {
                itemFound.getAction().setEnabled(enabled);
            }
        }

        return getMenuManger().getItems();
    }

    /**
     * Determines if the menu item associated with the specified
     * {@link DisplayFieldData} should be enabled.
     * 
     * @param fields
     *            {@link List} of fields to generate defined by the
     *            'mpe_generate_list' Apps_defaults token
     * @param data
     *            the specified {@link DisplayFieldData}
     * @return {code true} if the menu item should be enabled; {@code false},
     *         otherwise.
     */
    protected boolean menuItemEnabled(final List<String> fields,
            final DisplayFieldData data) {
        return (Collections.binarySearch(fields, data.name(),
                new Comparator<String>() {
                    @Override
                    public int compare(String o1, String o2) {
                        return o1.compareToIgnoreCase(o2);
                    }
                }) >= 0);
    }

    protected class DisplayFieldAction extends Action {
        private final DisplayFieldData data;

        private final MenuData menuData;

        public DisplayFieldAction(DisplayFieldData data, boolean enabled) {
            super("", getTexMap().get(data).type);
            this.menuData = getTexMap().get(data);
            String id = this.menuData.handler == null ? this.menuData.text
                    : this.menuData.handler;
            this.setActionDefinitionId(this.menuData.handler);
            this.setId(id);
            String s = getTexMap().get(data).text;
            String mnemonic = getTexMap().get(data).mnemonic;
            if (mnemonic == null || s == null) {
                setText(s);
            }
            int idx = s.indexOf(mnemonic);
            if (idx == -1) {
                setText(s);
            } else {
                setText(s.substring(0, idx) + '&' + s.substring(idx));
            }
            this.data = data;
            setEnabled(enabled);
        }

        @Override
        public void run() {
            if (menuData.handler == null) {
                SetDisplayField.setDisplayField(
                        EditorUtil.getActiveVizContainer(), data);
            } else {
                try {
                    ICommandService commandService = PlatformUI.getWorkbench()
                            .getService(ICommandService.class);
                    Command command = commandService
                            .getCommand(menuData.handler);
                    // This is to allow Save Level 2 menu option to be used
                    // while in Daily QC,
                    // but only while in DailyQC, and no other menu options.
                    if (command != null) {
                        boolean inDqc = isDailyQC();
                        boolean l2Cmd = isLevel2(command);
                        if ((!inDqc) && (!l2Cmd)) {
                            ExecutionEvent event = new ExecutionEvent(command,
                                    menuData.params, null, null);
                            command.executeWithChecks(event);
                        } else {
                            if ((inDqc) && (l2Cmd)) {
                                ExecutionEvent event = new ExecutionEvent(
                                        command, menuData.params, null, null);
                                command.executeWithChecks(event);
                            }

                        }
                    }
                } catch (CommandException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error excuting command", e);
                }
            }
        }

    }

    /**
     * @return The dynamic menu items
     */
    protected abstract DisplayFieldData[] getMenuItems();

    /**
     * @return The menu to be altered
     */
    protected abstract MenuManager getMenuManger();

    /**
     * @return The map of MenuData items
     */
    protected abstract Map<DisplayFieldData, MenuData> getTexMap();

    /**
     * Check to see if Daily QC is open
     * 
     * @return true if Daily QC is running otherwise is false
     */
    private boolean isDailyQC() {
        boolean dqc = false;
        MPEDisplayManager displayMan = MPEDisplayManager.getCurrent();
        if ((displayMan.getQcFreezeDialog() != null
                && displayMan.getQcFreezeDialog().isOpen())
                || (displayMan.getQcPrecipDialog() != null
                        && displayMan.getQcPrecipDialog().isOpen())
                || (displayMan.getQcTempDialog() != null
                        && displayMan.getQcTempDialog().isOpen())) {
            dqc = true;
        }

        return dqc;
    }

    /**
     * Check to see if menu command is for Save Level 2 data
     * 
     * @param cmd
     *            Menu command called
     * @return true if Menu option is Save Level 2
     * 
     */
    private boolean isLevel2(Command cmd) {
        boolean level2 = false;
        if (COMMAND_ID.equals(cmd.getId())) {
            level2 = true;
        }
        return level2;
    }

}
