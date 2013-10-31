package ohd.hseb.fcstservice;   

public interface Subject //Subject of the observer pattern
{
	public void addObserver(Observer observer);
	public void removeObserver(Observer observer);
	public void letKnowOfChanges();
}
