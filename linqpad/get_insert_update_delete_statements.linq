<Query Kind="Program">
  <NuGetReference>Npgsql</NuGetReference>
  <Namespace>Npgsql</Namespace>
</Query>

void Main()
{
	var tableList = new string[] { "table_names_here" };
	var connectionString = "User ID=postgres;Host=localhost;Port=5432;Database=postgres;Connection Lifetime=0;";
	var connection = new NpgsqlConnection(connectionString);
	connection.Open();
	GetTables(connection, tableList);
}

class TableColumn
{
	public string ColumnName { get; set; }
	public string DataType { get; set; }
	public bool IsPrimaryKey { get; set; }
	public bool IsNullable { get; set; }
}

class TableConstraints
{
	public string ColumnName { get; set; }
	public string ConstraintType { get; set; }
}

enum CodeType
{
	GetterSearch,
	GetterOne,
	Insert,
	InsertOnConflictUpdate,
	Update,
	Delete,
	ClassDef,
	ClassSetterDef
}

class Output
{
	public string TableName { get; set; }
	public CodeType Type {get;set;}
	public string GeneratedCode { get; set; }
}

string SnakeCaseToPascalCase(string snake)
{
	var result = "";
	var isUnderscore = false;
	for (int i = 0; i < snake.Length; i++)
	{
		if (i == 0)
		{
			result += char.ToUpper(snake[i]);
			continue;
		}
		if (isUnderscore)
		{
			result += char.ToUpper(snake[i]);
			isUnderscore = false;
			continue;
		}
		if (snake[i] == '_')
		{
			isUnderscore = true;
		}
		else
		{
			result += snake[i];
		}
	}
	return result;
}

string PgDataTypeToCSharp(string pgDataType)
{
	switch (pgDataType)
	{
		case "bigint":
			return "long";
		case "int":
		case "integer":
			return "int";
		case "timestamp":
		case "timestamp without time zone":
		case "date":
			return "DateTime";
		case "character varying":
			return "string";
		case "numeric":
			return "decimal";
		case "user-defined":
			return "string";
		default:
			return "FUCK";
	}
}

DataTable ExecuteQueryFillDataTable(string query, NpgsqlConnection conn)
{
	var command = new NpgsqlCommand(query, conn);
	var dataTable = new DataTable();
	var sqlDataAdapter = new NpgsqlDataAdapter(command);
	sqlDataAdapter.Fill(dataTable);
	return dataTable;
}

List<TableColumn> GetTableColumns(string tableName, NpgsqlConnection conn)
{
	var queryTable = "select * from information_schema.columns where table_name = '" + tableName + "'";
	var dataTable = ExecuteQueryFillDataTable(queryTable, conn);
	var cols = new List<TableColumn>();
	foreach (DataRow rc in dataTable.Rows)
	{
		cols.Add(new TableColumn
		{
			ColumnName = rc["column_name"].ToString(),
			DataType = rc["data_type"].ToString(),
			IsNullable = rc["is_nullable"].ToString() == "YES" ? true : false
		});
	}
	return cols;
}

List<TableConstraints> GetPrimaryKeys(string tableName, NpgsqlConnection conn)
{
	var queryTable = "SELECT c.column_name, c.data_type FROM information_schema.table_constraints tc JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema AND tc.table_name = c.table_name AND ccu.column_name = c.column_name WHERE constraint_type = 'PRIMARY KEY' and tc.table_name = '" + tableName + "';";
	var dataTable = ExecuteQueryFillDataTable(queryTable, conn);
	var cols = new List<TableConstraints>();
	foreach (DataRow rc in dataTable.Rows)
	{
		cols.Add(new TableConstraints
		{
			ColumnName = rc["column_name"].ToString(),
			ConstraintType = rc["data_type"].ToString().ToLowerInvariant()
		});
	}
	return cols;
}

void WriteClassDefinition(string tableName, List<TableColumn> cols, List<Output> output)
{
	var cSharpDefinitions = cols.Aggregate(string.Empty, (x, y) => x + @$"public {PgDataTypeToCSharp(y.DataType.ToLower())} {SnakeCaseToPascalCase(y.ColumnName)} {{get;set;}}" + "\r\n");
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.ClassDef,
		GeneratedCode = $@"
		public class {SnakeCaseToPascalCase(tableName)} {{
			{cSharpDefinitions}
		}}
	"
	});
	
	output.Add(new Output {
		TableName = tableName,
		Type = CodeType.ClassSetterDef,
		GeneratedCode = $@"
		while (await reader.ReadAsync()) {{
			
		}}
		"
	});
}

void WriteInsertDefinition(string tableName, List<TableColumn> cols, List<Output> output)
{
	var columnNames = cols.Select(x => x.ColumnName);
	var columnNamesAgg = columnNames.Aggregate(string.Empty, (x, y) => x + y + ", ").TrimEnd(' ').TrimEnd(',');
	var columnVariables = columnNames.Aggregate(string.Empty, (x, y) => x + "@" + SnakeCaseToPascalCase(y) + ", ").TrimEnd(' ').TrimEnd(',');
	var parameterList = cols.Aggregate(string.Empty, (x, y) => x + @$"cmd.Parameters.AddWithValue(""@{SnakeCaseToPascalCase(y.ColumnName)}"", param.{SnakeCaseToPascalCase(y.ColumnName)});" + "\r\n");
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.Insert,
		GeneratedCode = $@"
		try
		{{
			var param = <<<inParam>>>;
			var query = @""INSERT INTO {tableName} ({columnNamesAgg}) VALUES ({columnVariables});"";
			using var conn = await _dbConnectionFactory.CreateConnection();
	        using var cmd = new NpgsqlCommand(query, conn);
			{parameterList}
			await cmd.ExecuteNonQueryAsync();
		}}
		catch (Exception ex) 
		{{
			_logger.LogErrorException(ex, ""Error executing insert on {tableName}"");
		}}
		"
	});

}

void WriteInsertOnConflictUpdateDefinition(string tableName, List<TableColumn> cols, List<TableConstraints> constraints, List<Output> output)
{
	var columnNames = cols.Select(x => x.ColumnName);
	var columnNamesAgg = columnNames.Aggregate(string.Empty, (x, y) => x + y + ", ").TrimEnd(' ').TrimEnd(',');
	var columnVariables = columnNames.Aggregate(string.Empty, (x, y) => x + "@" + SnakeCaseToPascalCase(y) + ", ").TrimEnd(' ').TrimEnd(',');
	var columnSetters = cols.Aggregate(string.Empty, (x, y) => x + $"{y.ColumnName} = excluded.{y.ColumnName}, \r\n").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd(',');
	var primaryKeyStatements = constraints.Aggregate(string.Empty, (x, y) => x + @$"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)} AND ").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd('D').TrimEnd('N').TrimEnd('A');
	var parameterList = cols.Aggregate(string.Empty, (x, y) => x + @$"cmd.Parameters.AddWithValue(""@{SnakeCaseToPascalCase(y.ColumnName)}"", param.{SnakeCaseToPascalCase(y.ColumnName)});" + "\r\n");
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.InsertOnConflictUpdate,
		GeneratedCode = $@"
		try
		{{
			var param = <<<inParam>>>;
			var query = @""INSERT INTO {tableName} ({columnNamesAgg}) VALUES ({columnVariables}) ON CONFLICT DO UPDATE SET {columnSetters} WHERE {primaryKeyStatements};"";
			using var conn = await _dbConnectionFactory.CreateConnection();
	        using var cmd = new NpgsqlCommand(query, conn);
			{parameterList}
			await cmd.ExecuteNonQueryAsync();
		}}
		catch (Exception ex) 
		{{
			_logger.LogErrorException(ex, ""Error executing insert on {tableName}"");
		}}
		"
	});

}

void WriteUpdateDefinition(string tableName, List<TableColumn> cols, List<TableConstraints> constraints, List<Output> output)
{
	var columnSetters = cols.Aggregate(string.Empty, (x, y) => x + $"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)}, \r\n").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd(',');
	var primaryKeyStatements = constraints.Aggregate(string.Empty, (x, y) => x + @$"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)} AND ").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd('D').TrimEnd('N').TrimEnd('A');
	var parameterList = cols.Aggregate(string.Empty, (x, y) => x + @$"cmd.Parameters.AddWithValue(""@{SnakeCaseToPascalCase(y.ColumnName)}"", param.{SnakeCaseToPascalCase(y.ColumnName)});" + "\r\n");
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.Update,
		GeneratedCode = $@"
		try
		{{
			var param = <<<inParam>>>;
			var query = @""UPDATE {tableName} SET {columnSetters} WHERE {primaryKeyStatements};"";
			using var conn = await _dbConnectionFactory.CreateConnection();
	        using var cmd = new NpgsqlCommand(query, conn);
			{parameterList}
			await cmd.ExecuteNonQueryAsync();
		}}
		catch (Exception ex) 
		{{
			_logger.LogErrorException(ex, ""Error executing update on {tableName}"");
		}}
		"
	});
}

void WriteDeleteDefinition(string tableName, List<TableColumn> cols, List<TableConstraints> constraints, List<Output> output)
{
	var columnSetters = cols.Aggregate(string.Empty, (x, y) => x + $"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)}, \r\n").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd(',');
	var primaryKeyStatements = constraints.Aggregate(string.Empty, (x, y) => x + @$"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)} AND ").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd('D').TrimEnd('N').TrimEnd('A');
	var parameterList = constraints.Aggregate(string.Empty, (x, y) => x + @$"cmd.Parameters.AddWithValue(""@{SnakeCaseToPascalCase(y.ColumnName)}"", param.{SnakeCaseToPascalCase(y.ColumnName)});" + "\r\n");
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.Delete,
		GeneratedCode = $@"
		try
		{{
			var param = <<<inParam>>>;
			var query = @""DELETE FROM {tableName} WHERE {primaryKeyStatements};"";
			using var conn = await _dbConnectionFactory.CreateConnection();
	        using var cmd = new NpgsqlCommand(query, conn);
			{parameterList}
			await cmd.ExecuteNonQueryAsync();
		}}
		catch (Exception ex) 
		{{
			_logger.LogErrorException(ex, ""Error executing delete on {tableName}"");
		}}
		"
	});
}

void WriteGetterDefinition(string tableName, List<TableColumn> cols, List<TableConstraints> pkeys, List<Output> output)
{
	var columnNameList = cols.Aggregate(string.Empty, (x, y) => x + $"{y.ColumnName}, \r\n").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd(',');
	var primaryKeyStatements = pkeys.Aggregate(string.Empty, (x, y) => x + @$"{y.ColumnName} = @{SnakeCaseToPascalCase(y.ColumnName)} AND ").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd('D').TrimEnd('N').TrimEnd('A');
	var parameterList = cols.Aggregate(string.Empty, (x, y) => x + @$"cmd.Parameters.AddWithValue(""@{SnakeCaseToPascalCase(y.ColumnName)}"", param.{SnakeCaseToPascalCase(y.ColumnName)});" + "\r\n");
	var searchParameterList = cols.Aggregate(string.Empty, (x, y) => x + @$"(@{SnakeCaseToPascalCase(y.ColumnName)} IS NULL OR {y.ColumnName} LIKE '%' || @{SnakeCaseToPascalCase(y.ColumnName)} || '%') OR " + "\r\n").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd('R').TrimEnd('O').TrimEnd(' ');
	Func<TableColumn, string> nullableReader = (TableColumn c) => {
		if (c.IsNullable)
		{
			return $@"reader.IsDbNull(""{c.ColumnName}"") ? null : ";
		}
		return "";
	};
	var readerOutputs = cols.Aggregate(string.Empty, (x, y) => x + @$"{SnakeCaseToPascalCase(y.ColumnName)} = {nullableReader(y)}reader.GetFieldValue<{PgDataTypeToCSharp(y.DataType)}>(""{y.ColumnName}""), ").TrimEnd('\n').TrimEnd('\r').TrimEnd(' ').TrimEnd(',');
	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.GetterOne,
		GeneratedCode = $@"
	    NpgsqlDataReader reader = null;
        var results = new List<{SnakeCaseToPascalCase(tableName)}>();
        
        try
        {{
            var query = @""SELECT {columnNameList} FROM {tableName};"";
            using var conn = await _dbConnectionFactory.CreateConnection();
            using var cmd = new NpgsqlCommand(query, conn);
            
            reader = await cmd.ExecuteReaderAsync();
            if (reader.HasRows)
            {{
                while (await reader.ReadAsync())
                {{
                    var result = new {SnakeCaseToPascalCase(tableName)}
                    {{
                        {readerOutputs}
                    }};
                    results.Add(result);
                }}
            }}

            return results;
        }}
        catch (Exception ex)
        {{
            _logger.LogErrorException(ex, ""Error executing select on {tableName}"");
        }}
        finally
        {{
            if (reader != null && !reader.IsClosed)
            {{
                await reader.CloseAsync();
            }}
        }}

        return null;
		"
	});

	output.Add(new Output
	{
		TableName = tableName,
		Type = CodeType.GetterSearch,
		GeneratedCode = $@"
		var dataTable = new DataTable();
		try
		{{
			var param = <<<inParam>>>;
			var query = @""SELECT {columnNameList} FROM {tableName} WHERE {searchParameterList};"";
			using var conn = await _dbConnectionFactory.CreateConnection();
	        using var cmd = new NpgsqlCommand(query, conn);
			{parameterList}
			var sqlDataAdapter = new NpgsqlDataAdapter(cmd);
			sqlDataAdapter.Fill(dataTable);
		}}
		catch (Exception ex) 
		{{
			_logger.LogErrorException(ex, ""Error executing select on {tableName}"");
		}}
		"
	});
}

void GetTables(NpgsqlConnection conn, string[] tableList)
{
	var query = "select * from information_schema.tables where table_name in (" + tableList.Aggregate(string.Empty, (x, y) => x + "'" + y + "'" + ", ").TrimEnd(' ').TrimEnd(',') + ");";
	var dataTable = ExecuteQueryFillDataTable(query, conn);

	foreach (DataRow row in dataTable.Rows)
	{
		var tableName = row["table_name"].ToString();
		var cols = GetTableColumns(tableName, conn);
		var pkeys = GetPrimaryKeys(tableName, conn);

		var results = new List<Output>();

		WriteClassDefinition(tableName, cols, results);
		WriteInsertDefinition(tableName, cols, results);
		WriteInsertOnConflictUpdateDefinition(tableName, cols, pkeys, results);
		WriteUpdateDefinition(tableName, cols, pkeys, results);
		WriteDeleteDefinition(tableName, cols, pkeys, results);
		WriteGetterDefinition(tableName, cols, pkeys, results);

		Console.WriteLine(results);
		//Console.WriteLine(@"Updater: ");
		//Console.WriteLine(@"Deleter: ");
	}
}

