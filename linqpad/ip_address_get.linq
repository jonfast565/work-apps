<Query Kind="Program">
  <Namespace>System.Net</Namespace>
  <Namespace>System.Net.Sockets</Namespace>
  <Namespace>System.Net.NetworkInformation</Namespace>
</Query>

void Main()
{
	var result = GetAllAddresses();
	Console.WriteLine(result);
}

public static List<string> GetAllAddresses(
	NetworkInterfaceType type = NetworkInterfaceType.Ethernet, 
	AddressFamily family = AddressFamily.InterNetwork)
{
	var addresses = NetworkInterface.GetAllNetworkInterfaces()
	.Where(x => x.NetworkInterfaceType == type && x.OperationalStatus == OperationalStatus.Up)
	.SelectMany(y => y.GetIPProperties().UnicastAddresses)
	.Where(y => y.Address.AddressFamily == family)
	.Select(x => x.Address.ToString())
	.ToList();
	return addresses;
}