package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class VerificationResponseTypeJTableRowData extends AbstractJTableRowData
{
	private String verifRespType;
	private String missingRep = null;
	
	public VerificationResponseTypeJTableRowData()
	{
	}
	
	public VerificationResponseTypeJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Verification Response Type", CellType.STRING, getVerifRespType(), missingRep));
	}

	public String getVerifRespType() {
		return verifRespType;
	}

	public void setVerifRespType(String verifRespType) {
		this.verifRespType = verifRespType;
	}
}
