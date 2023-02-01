<Query Kind="Program">
  <Namespace>System.Security.Cryptography.X509Certificates</Namespace>
  <Namespace>System.Security.Cryptography</Namespace>
</Query>

void Main()
{
	
}

public static byte[] EncryptDataOaepSha1(X509Certificate2 cert, byte[] data)
{
	using (RSA rsa = cert.GetRSAPublicKey())
	{
		return rsa.Encrypt(data, RSAEncryptionPadding.OaepSHA512);
	}
}

public static byte[] DecryptDataOaepSha1(X509Certificate2 cert, byte[] data)
{
	using (RSA rsa = cert.GetRSAPrivateKey())
	{
		return rsa.Decrypt(data, RSAEncryptionPadding.OaepSHA512);
	}
}