<Query Kind="Program">
  <NuGetReference>AWSSDK.CloudWatch</NuGetReference>
  <NuGetReference>AWSSDK.CloudWatchLogs</NuGetReference>
  <Namespace>System.Collections.Concurrent</Namespace>
  <Namespace>System.Timers</Namespace>
  <Namespace>Amazon.CloudWatch</Namespace>
  <Namespace>Amazon.CloudWatch.Model</Namespace>
</Query>

void Main()
{
	
}

public interface ICloudMetricSettings 
{
	bool EnableCloudWatch { get; set; }
	string CloudWatchNamespace { get; set; }
	int CloudWatchUpdateIntervalInMinute { get; set; }
}

public class CloudMetricSettings : ICloudMetricSettings
{
	public bool EnableCloudWatch { get; set; }
	public string CloudWatchNamespace { get; set; }
	public int CloudWatchUpdateIntervalInMinute { get; set; }
}

public class CloudMetricMessage 
{
	public string Key { get; set; }
	public int Count { get; set; }
	public StandardUnit Unit { get; set; }
}

public class CloudMetricPropagationStartChunk 
{
	public string PropagationKey { get; set; }
	public string Key { get; set; }
	public DateTime Start { get; set; }
}

public class CloudMetricPropagationMessage 
{
	public string PropagationKey { get;set; }
	public string Key { get; set; }
	public DateTime Start { get; set; }
	public DateTime End { get; set; }
	public int DurationMs => (End - Start).Milliseconds;
}

public class CloudMetricWatcher 
{
	
}
