<Query Kind="Program">
  <NuGetReference>StackExchange.Redis</NuGetReference>
  <Namespace>StackExchange.Redis</Namespace>
</Query>

void Main()
{
	var redisServer = "server name";
	var redisDbNumber = 5;
	
	var redisConnection = ConnectionMultiplexer.Connect(redisServer);
	var database = redisConnection.GetDatabase(redisDbNumber);
	var subscriberConnection = redisConnection.GetSubscriber();
	var channelName = "ChatChannel";
	
	subscriberConnection.Subscribe(channelName, HandleSubscription);
	
	while (true) 
	{
		Console.WriteLine("Send a message > ");
		var value = Console.ReadLine();
		subscriberConnection.Publish(channelName, value); 
	}
}

void HandleSubscription(RedisChannel channel, RedisValue value)
{
	Console.WriteLine($"Got: {value}");
}
