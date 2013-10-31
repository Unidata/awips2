package ohd.hseb.monitor.manager;

import ohd.hseb.monitor.MonitorMessage;

public interface Receiver
{
  public void receive(MonitorMessage message);
}
