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
package com.raytheon.viz.gfe.dialogs.isc;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.progress.UIJob;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest.IscQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * The ISC Request dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2009  1995      lvenable     Initial port
 * Oct 24. 2008  1287      rferrel      Made dialog non-blocking.
 * Dec 28, 2012  DR15587   jzeng        Query weather elements from fcst DB
 * Aug 14, 2015  4750      dgilling     Remove broken query.
 * Nov 30, 2015  5129      dgilling     Support new IFPClient.
 * Aug 04, 2016  5788      dgilling     Initialize data off UI thread.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ISCRequestReplyDlg extends CaveSWTDialog {

    private class RequestISCDataJob extends Job {

        RequestISCDataJob() {
            super("Requesting server information from IRT server.");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            IscQueryResponse response = dataManager.doIscRequestQuery();

            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }

            if (!response.isEmpty()) {
                initializeData(response);
            } else {
                iscAvailable = false;
            }

            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }

            populateJob.schedule();
            return Status.OK_STATUS;
        }
    }

    private class PopulateDialogJob extends UIJob {

        PopulateDialogJob() {
            super("Populating ISCRequestReplyDlg controls");
            setSystem(true);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }

            if (iscAvailable) {
                populateWEList();
            }
            populateDomainList();

            if (!getShell().isDisposed()) {
                getShell().setCursor(
                        getDisplay().getSystemCursor(SWT.CURSOR_ARROW));
            }

            return Status.OK_STATUS;
        }
    }

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ISCRequestReplyDlg.class);

    private final DataManager dataManager;

    private final RequestISCDataJob requestJob;

    private final PopulateDialogJob populateJob;

    private ToggleSelectList domainList;

    private ToggleSelectList gridSrcList;

    private ToggleSelectList weatherElemList;

    private List<String> weList;

    private Map<String, Map<String, List<Map<String, String>>>> domainDict;

    private Map<String, Map<String, String>> serverDictT2S;

    private volatile boolean iscAvailable;

    private Button makeRequestBtn;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ISCRequestReplyDlg(DataManager dataManager, Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("ISC Request/Reply");

        this.dataManager = dataManager;
        this.iscAvailable = this.dataManager.requestISC();

        this.requestJob = new RequestISCDataJob();
        this.populateJob = new PopulateDialogJob();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createLabelsAndLists();
        createBottomButtons();

        if (iscAvailable) {
            requestJob.schedule();
            getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        } else {
            statusHandler
                    .error("Server Problem: Unable to get server info from iscRequestQuery -IRT Address/requestISC not enabled. No ISC data available");
            populateDomainList();
        }
    }

    private void createLabelsAndLists() {
        final Composite listComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 15;
        listComp.setLayout(gl);
        listComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label domainLbl = new Label(listComp, SWT.CENTER);
        domainLbl.setText("Domain");
        domainLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label gridSrcLbl = new Label(listComp, SWT.CENTER);
        gridSrcLbl.setText("Grid Source");
        gridSrcLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label weatherLbl = new Label(listComp, SWT.CENTER);
        weatherLbl.setText("Weather Elements");
        weatherLbl.setLayoutData(gd);

        domainList = new ToggleSelectList(listComp, SWT.BORDER | SWT.V_SCROLL);
        domainList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                gridSrcList.removeAll();

                String[] selectedDomains = domainList.getSelection();
                for (String domain : selectedDomains) {
                    for (String str : serverDictT2S.keySet()) {
                        if (str.startsWith(domain)) {
                            gridSrcList.add(str);
                        }
                    }
                }

                boolean enableBtn = (domainList.getSelectionCount() > 0)
                        && (gridSrcList.getSelectionCount() > 0);
                makeRequestBtn.setEnabled(enableBtn);
            }
        });

        GC gc = new GC(domainList);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        Rectangle trim = domainList.computeTrim(0, 0, (charWidth * 30),
                (domainList.getItemHeight() * 12));
        int listWidth = trim.width;
        int listHeight = trim.height;

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = listHeight;
        gd.minimumWidth = listWidth;
        gd.minimumHeight = listHeight;
        domainList.setLayoutData(gd);

        gridSrcList = new ToggleSelectList(listComp, SWT.BORDER | SWT.V_SCROLL);
        gridSrcList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean enableBtn = (domainList.getSelectionCount() > 0)
                        && (gridSrcList.getSelectionCount() > 0);
                makeRequestBtn.setEnabled(enableBtn);
            }
        });
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = listHeight;
        gd.minimumWidth = listWidth;
        gd.minimumHeight = listHeight;
        gridSrcList.setLayoutData(gd);

        weatherElemList = new ToggleSelectList(listComp, SWT.BORDER
                | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = listHeight;
        gd.minimumWidth = listWidth;
        gd.minimumHeight = listHeight;
        weatherElemList.setLayoutData(gd);

        if (!iscAvailable) {
            listComp.setEnabled(false);
        }
    }

    private void createBottomButtons() {
        addSeparator(shell);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        makeRequestBtn = new Button(buttonComp, SWT.PUSH);
        makeRequestBtn.setText("Make Request");
        makeRequestBtn.setEnabled(false);
        makeRequestBtn.setLayoutData(gd);
        makeRequestBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                makeRequest();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("&Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.close();
            }
        });
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void initializeData(IscQueryResponse response) {
        weList = new ArrayList<>(response.getRequestedParms());
        Collections.sort(weList);

        domainDict = response.getDomainDict();
        serverDictT2S = response.getServerDictT2S();

        // output the list of servers and their priority
        StringBuilder s = new StringBuilder("\n");
        for (String key : domainDict.keySet()) {
            s.append("DOMAIN=").append(key).append('\n');
            List<Map<String, String>> servers = (List<Map<String, String>>) domainDict
                    .get(key);
            for (Map<String, String> server : servers) {
                s.append("  mhs=").append(server.get("mhsid")).append(" host=")
                        .append(server.get("host")).append(" port=")
                        .append(server.get("port")).append('\n');
            }

        }
        statusHandler.handle(Priority.EVENTB,
                "DomainDict servers:" + s.toString());
    }

    private void populateDomainList() {
        if (!domainList.isDisposed()) {
            domainList.removeAll();
            if (iscAvailable) {
                domainList.setItems(domainDict.keySet().toArray(new String[0]));
            } else {
                domainList.add("ISC Request/Reply not avaliable");
            }
            domainList.deselectAll();
        }
    }

    private void populateWEList() {
        if (!weatherElemList.isDisposed()) {
            weatherElemList.removeAll();
            weatherElemList.setItems(weList.toArray(new String[0]));
            weatherElemList.selectAll();
            weatherElemList.deselectAll();
        }
    }

    public void makeRequest() {
        if (iscAvailable) {
            if (ArrayUtils.isEmpty(domainList.getSelection())) {
                statusHandler.handle(Priority.PROBLEM,
                        "No Domain has been selected");
                return;
            }
            List<String> selectedServers = Arrays.asList(gridSrcList
                    .getSelection());
            if (selectedServers.isEmpty()) {
                statusHandler.handle(Priority.PROBLEM,
                        "No Grid Source has been selected");
                return;
            }
            List<String> weatherElements = null;
            if (ArrayUtils.isEmpty(weatherElemList.getSelection())) {
                weatherElements = Arrays.asList(weatherElemList.getItems());
            } else {
                weatherElements = Arrays.asList(weatherElemList.getSelection());
            }

            Document doc = null;
            try {
                doc = IrtAccess.getIscRequestXML(selectedServers,
                        weatherElements, serverDictT2S);
            } catch (DOMException | ParserConfigurationException e) {
                statusHandler.error(
                        "Unable to create XML document for request.", e);
                return;
            }

            // output the list of servers and their priority
            StringBuilder s = new StringBuilder("\n");
            for (String key : domainDict.keySet()) {
                s.append("DOMAIN=").append(key).append('\n');
                for (String serverT : selectedServers) {
                    Map<String, String> server = serverDictT2S.get(serverT);
                    if (key.equals(server.get("site"))) {
                        s.append("  mhs=").append(server.get("mhsid"))
                                .append(" host=").append(server.get("host"))
                                .append(" port=").append(server.get("port"))
                                .append('\n');
                    }
                }
            }
            statusHandler.handle(Priority.EVENTB,
                    "Chosen request servers:" + s.toString());

            // send to ifpServer
            String xmlreq = StringUtils.EMPTY;
            try {
                xmlreq = IrtAccess.convertXMLToString(doc);
            } catch (TransformerException e) {
                statusHandler.error(
                        "Unable to write XML document for request.", e);
                return;
            }

            dataManager.makeISCRequest(xmlreq);
        }
    }

    private static class IrtAccess {

        private IrtAccess() {
            throw new AssertionError();
        }

        public static Document getIscRequestXML(List<String> selectedServers,
                List<String> weatherElements,
                Map<String, Map<String, String>> serverDictT2S)
                throws DOMException, ParserConfigurationException {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();

            Document doc = builder.newDocument();
            Element iscReqE = doc.createElement("iscrequest");
            doc.appendChild(iscReqE);
            List<Map<String, String>> destinations = new ArrayList<>();
            for (String serverT : selectedServers) {
                Map<String, String> server = serverDictT2S.get(serverT);
                destinations.add(server);
            }
            addDestinationXML(iscReqE, destinations);
            Element welistE = doc.createElement("welist");
            iscReqE.appendChild(welistE);
            for (String we : weatherElements) {
                Element weE = doc.createElement("parm");
                weE.appendChild(doc.createTextNode(we));
                welistE.appendChild(weE);
            }

            return doc;
        }

        public static String convertXMLToString(Document doc)
                throws TransformerException {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer transformer = tFactory.newTransformer();

            DOMSource source = new DOMSource(doc);
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            transformer.transform(source, result);
            return writer.getBuffer().toString();
        }

        private static Element addDestinationXML(Element root,
                List<Map<String, String>> serverInfos) {
            Element destinationsE = root.getOwnerDocument().createElement(
                    "destinations");
            root.appendChild(destinationsE);
            for (Map<String, String> serverInfo : serverInfos) {
                addAddressXML(destinationsE, serverInfo);
            }
            return destinationsE;
        }

        private static Element addAddressXML(Element root,
                Map<String, String> serverInfo) {
            Element addressE = root.getOwnerDocument().createElement("address");
            root.appendChild(addressE);

            Element mhsidE = root.getOwnerDocument().createElement("mhsid");
            String mhsidText = MapUtils.getString(serverInfo, "mhsid", "?");
            mhsidE.appendChild(root.getOwnerDocument()
                    .createTextNode(mhsidText));
            addressE.appendChild(mhsidE);

            Element serverE = root.getOwnerDocument().createElement("server");
            String serverText = MapUtils.getString(serverInfo, "host", "?");
            serverE.appendChild(root.getOwnerDocument().createTextNode(
                    serverText));
            addressE.appendChild(serverE);

            Element portE = root.getOwnerDocument().createElement("port");
            String portText = MapUtils.getString(serverInfo, "port", "?");
            portE.appendChild(root.getOwnerDocument().createTextNode(portText));
            addressE.appendChild(portE);

            Element protocolE = root.getOwnerDocument().createElement(
                    "protocol");
            String protocolText = MapUtils.getString(serverInfo, "protocol",
                    "?");
            protocolE.appendChild(root.getOwnerDocument().createTextNode(
                    protocolText));
            addressE.appendChild(protocolE);

            Element siteE = root.getOwnerDocument().createElement("site");
            String siteText = MapUtils.getString(serverInfo, "site", "?");
            siteE.appendChild(root.getOwnerDocument().createTextNode(siteText));
            addressE.appendChild(siteE);

            return addressE;
        }
    }

    @Override
    protected void disposed() {
        requestJob.cancel();
        populateJob.cancel();
        super.disposed();
    }
}
