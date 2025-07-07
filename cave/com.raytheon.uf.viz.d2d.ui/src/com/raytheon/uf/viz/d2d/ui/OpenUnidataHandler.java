package com.raytheon.uf.viz.d2d.ui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.browser.IWebBrowser;

import java.net.URL;


public class OpenUnidataHandler extends AbstractHandler {
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        try {
            IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
            IWebBrowser browser = browserSupport.createBrowser(
                    IWorkbenchBrowserSupport.AS_VIEW,
                    "unidataBrowser",
                    "Unidata",
                    "Unidata Browser");
            browser.openURL(new URL("https://unidata.github.io/awips2/"));
        } catch (Exception e) {
            throw new ExecutionException("Failed to open Unidata website", e);
        }
        return null;
    }
}