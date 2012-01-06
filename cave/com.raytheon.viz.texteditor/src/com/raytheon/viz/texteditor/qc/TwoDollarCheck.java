package com.raytheon.viz.texteditor.qc;

public class TwoDollarCheck implements IQCCheck {

    @Override
    public String runQC(String header, String body, String nnn) {
        String errorMsg = "";
        boolean hasDollar = false;
        String[] separtedLines = body.split("\n");
        for (int i = separtedLines.length - 1; i >= 15; i--) {
            String line = separtedLines[i];
            if (line.startsWith("*") || line.startsWith("LAT...LON")) {
                break;
            }

            if (line.equals("$$")) {
                hasDollar = true;
                break;
            }
        }

        if (separtedLines.length > 15 && !hasDollar) {
            errorMsg += "No $$ found at the bottom.\n";
        }
        return errorMsg;
    }

}
