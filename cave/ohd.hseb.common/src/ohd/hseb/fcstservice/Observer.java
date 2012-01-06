package ohd.hseb.fcstservice;  

public interface Observer //Observer of the observer pattern
{
	//There is no need of pasing the subject as regardless of the subject, all the tables will be read
	//Just in case we might need independent subject behaviour in the future, design doesn't have to be changed.
	public void update(Subject sub);
}
