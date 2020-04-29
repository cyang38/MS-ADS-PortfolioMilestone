
/****** Object:  Database ist722_mafudge_cb1_dw    Script Date: 10/13/2019 1:11:17 PM ******/
/*
Kimball Group, The Microsoft Data Warehouse Toolkit
Generate a database from the datamodel worksheet, version: 4

You can use this Excel workbook as a data modeling tool during the logical design phase of your project.
As discussed in the book, it is in some ways preferable to a real data modeling tool during the inital design.
We expect you to move away from this spreadsheet and into a real modeling tool during the physical design phase.
The authors provide this macro so that the spreadsheet isn't a dead-end. You can 'import' into your
data modeling tool by generating a database using this script, then reverse-engineering that database into
your tool.

Uncomment the next lines if you want to drop and create the database
*/
/*
DROP DATABASE ist722_mafudge_cb1_dw
GO
CREATE DATABASE ist722_mafudge_cb1_dw
GO
ALTER DATABASE ist722_mafudge_cb1_dw
SET RECOVERY SIMPLE
GO
*/
USE ist722_mafudge_cb1_dw
go

-- Create a schema to hold user views (set schema name on home page of workbook).
-- It would be good to do this only if the schema doesn't exist already.

--drop view FudgeComp.Customers
--drop view FudgeComp.Date
--drop view FudgeComp.Orders

--DROP SCHEMA FudgeComp
--GO

--CREATE SCHEMA FudgeComp
--GO

/* Drop table FudgeComp.FactShipment */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.FactShipment') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.FactShipment 
;

/* Drop table FudgeComp.FactRisk */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.FactRisk') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.FactRisk 
;

/*/* Drop table FudgeComp.DimOrders */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.DimOrders') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.DimOrders 
;

/* Create table FudgeComp.DimOrders */
CREATE TABLE FudgeComp.DimOrders (
   [OrderKey]  int IDENTITY  NOT NULL
,  [OrderID]  int    NULL
,  [OrderDateKey]  int    NULL
,  [ShippedDateKey]  int    NULL
,  [CreditCardID]  int    NULL
,  [CustomerID]  int    NULL
,  [ShipVia]  varchar(42)  DEFAULT 'N/A'  NULL
,  [OrderAmount]  numeric(30,4)    NULL
,  [Customer_Source]  nvarchar(20)    NULL
,  [RowIsCurrent]  bit  DEFAULT 1 NOT NULL
,  [RowStartDate]  datetime  DEFAULT '12/31/1899' NOT NULL
,  [RowEndDate]  datetime  DEFAULT '12/31/9999' NOT NULL
,  [RowChangeReason]  nvarchar(200)   NULL
, CONSTRAINT [PK_FudgeComp.DimOrders] PRIMARY KEY CLUSTERED 
( [OrderKey] )
) ON [PRIMARY]
;

SET IDENTITY_INSERT FudgeComp.DimOrders ON
;
INSERT INTO FudgeComp.DimOrders (OrderKey, OrderID, OrderDateKey, ShippedDateKey, CreditCardID, CustomerID, ShipVia, OrderAmount, 
	Customer_Source, RowIsCurrent, RowStartDate, RowEndDate, RowChangeReason)
VALUES (-1, -1, -1, -1, -1, -1, 'None', -1, 'None', 1, '12/31/1899', '12/31/9999', 'N/A')
;
SET IDENTITY_INSERT FudgeComp.DimOrders OFF
;
*/
/* Drop table FudgeComp.DimCustomers */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.DimCustomers') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.DimCustomers 
;

/* Create table FudgeComp.DimCustomers */
CREATE TABLE FudgeComp.DimCustomers (
   [CustomerKey]  int IDENTITY  NOT NULL
,  [CustomerSource]  varchar(9)  NULL
,  [email] varchar(200) NULL
,  [CustomerID]  int   NULL
,  [CustomerEmail]  varchar(200)   NULL
,  [CustomerFirstName]  varchar(50)    NULL
,  [CustomerLastName]  varchar(50)    NULL
,  [CustomerAddress]  varchar(1000)    NULL
,  [CustomerZipCode]  varchar(42)    NULL
,  [accountid] int NULL
,  [accountemail] varchar(200)  NULL
,  [accountfirstname] varchar(50) NULL
,  [accountlastname] varchar(50) NULL
,  [accountaddress] varchar(1000) NULL
,  [accountzipcode] char(5)  NULL
,  [RowIsCurrent]  bit  DEFAULT 1 NOT NULL
,  [RowStartDate]  datetime  DEFAULT '12/31/1899' NOT NULL
,  [RowEndDate]  datetime  DEFAULT '12/31/9999' NOT NULL
,  [RowChangeReason]  nvarchar(200)   NULL
, CONSTRAINT [PK_FudgeComp.DimCustomers] PRIMARY KEY CLUSTERED 
( [CustomerKey] )
) ON [PRIMARY]
;


SET IDENTITY_INSERT FudgeComp.DimCustomers ON
;
INSERT INTO FudgeComp.DimCustomers (CustomerKey, CustomerSource, email, CustomerID, CustomerEmail, CustomerFirstName, CustomerLastName, 
	CustomerAddress, CustomerZipCode, accountid, accountemail,accountfirstname,accountlastname,accountaddress,accountzipcode,
	RowIsCurrent, RowStartDate, RowEndDate, RowChangeReason)
VALUES (-1, 'None','None', -1, 'None', 'None', 'None', 'None', 'None',-1, 'None', 'None', 'None', 'None', 'None', 1, '12/31/1899', '12/31/9999', 'N/A')
;
SET IDENTITY_INSERT FudgeComp.DimCustomers OFF
;

/* Drop table FudgeComp.DimDate */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.DimDate') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.DimDate 
;

/* Create table FudgeComp.DimDate */
CREATE TABLE FudgeComp.DimDate (
   [DateKey]  int IDENTITY  NOT NULL
,  [Date]  datetime   NULL
,  [FullDateUSA]  nchar(11)   NOT NULL
,  [DayOfWeek]  tinyint   NOT NULL
,  [DayName]  nchar(10)   NOT NULL
,  [DayOfMonth]  tinyint   NOT NULL
,  [DayOfYear]  int   NOT NULL
,  [WeekOfYear]  tinyint   NOT NULL
,  [MonthName]  nchar(10)   NOT NULL
,  [MonthOfYear]  tinyint   NOT NULL
,  [Quarter]  tinyint   NOT NULL
,  [QuarterName]  nchar(10)   NOT NULL
,  [Year]  int   NOT NULL
,  [IsWeekday]  bit  DEFAULT 0 NOT NULL
, CONSTRAINT [PK_FudgeComp.DimDate] PRIMARY KEY CLUSTERED 
( [DateKey] )
) ON [PRIMARY]
;

SET IDENTITY_INSERT FudgeComp.DimDate ON
;
INSERT INTO FudgeComp.DimDate (DateKey, Date, FullDateUSA, DayOfWeek, DayName, DayOfMonth, DayOfYear, WeekOfYear, MonthName, MonthOfYear, Quarter, QuarterName, Year, IsWeekday)
VALUES (-1, '', 'Unk date', 0, 'Unk date', 0, 0, 0, 'Unk month', 0, 0, 'Unk qtr', 0, 0)
;
SET IDENTITY_INSERT FudgeComp.DimDate OFF
;

/* Drop table FudgeComp.DimCreditCardInfo */
IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'FudgeComp.DimCreditCardInfo') AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
DROP TABLE FudgeComp.DimCreditCardInfo 
;

/* Create table FudgeComp.DimCreditCardInfo */
CREATE TABLE FudgeComp.DimCreditCardInfo (
   [CreditcardKey]  int IDENTITY  NOT NULL
,  [CreditcardID]  int   NULL
,  [CreditcardNumber]  varchar(50)   NULL
,  [ExpirationDateKey]  int  NULL
,  [RowIsCurrent]  bit  DEFAULT 1 NOT NULL
,  [RowStartDate]  datetime  DEFAULT '12/31/1899' NOT NULL
,  [RowEndDate]  datetime  DEFAULT '12/31/9999' NOT NULL
,  [RowChangeReason]  nvarchar(200)   NULL
, CONSTRAINT [PK_FudgeComp.DimCreditCardInfo] PRIMARY KEY CLUSTERED 
( [CreditcardKey] )
) ON [PRIMARY]
;


SET IDENTITY_INSERT FudgeComp.DimCreditCardInfo ON
;
INSERT INTO FudgeComp.DimCreditCardInfo (CreditcardKey, CreditcardID, CreditcardNumber, ExpirationDateKey, RowIsCurrent, RowStartDate, RowEndDate, RowChangeReason)
VALUES (-1, -1, 'none', -1, 1, '12/31/1899', '12/31/9999', 'N/A')
;
SET IDENTITY_INSERT FudgeComp.DimCreditCardInfo OFF
;

/* Create table FudgeComp.FactShipment */
CREATE TABLE FudgeComp.FactShipment (
   [OrderKey]  int   NOT NULL
,  [CustomerKey]  int   NOT NULL
,  [OrderDateKey]  int   NOT NULL
,  [ShippedDateKey]  int   NOT NULL
,  [OrderToShipInDays]  int   NOT NULL
,  [MartOrFlix]  varchar(20)   NOT NULL
, CONSTRAINT [PK_FudgeComp.FactShipment] PRIMARY KEY NONCLUSTERED 
( [OrderKey] ,[MartOrFlix])
)
;


/* Create table FudgeComp.FactRisk */
CREATE TABLE FudgeComp.FactRisk (
   [OrderKey]  int   NOT NULL
,  [CreditCardKey]  int   NOT NULL
,  [CustomerKey]  int   NOT NULL
,  [OrderDateKey]  int   NOT NULL
,  [ExpirationDateKey]  int   NOT NULL
,  [ExpirationInDays]  int   NOT NULL
,  [OrderAmount]  numeric(30,4)   NOT NULL
,  [ApprovalOrNot]  bit   NOT NULL
, CONSTRAINT [PK_FudgeComp.FactRisk] PRIMARY KEY NONCLUSTERED 
( [OrderKey] )
) ON [PRIMARY]
;


/*ALTER TABLE FudgeComp.FactShipment ADD CONSTRAINT
   FK_FudgeComp_FactShipment_OrderKey FOREIGN KEY
   (
   OrderKey
   ) REFERENCES FudgeComp.DimOrders
   ( OrderKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;*/
 
ALTER TABLE FudgeComp.FactShipment ADD CONSTRAINT
   FK_FudgeComp_FactShipment_CustomerKey FOREIGN KEY
   (
   CustomerKey
   ) REFERENCES FudgeComp.DimCustomers
   ( CustomerKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
ALTER TABLE FudgeComp.FactShipment ADD CONSTRAINT
   FK_FudgeComp_FactShipment_OrderDateKey FOREIGN KEY
   (
   OrderDateKey
   ) REFERENCES FudgeComp.DimDate
   ( DateKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
ALTER TABLE FudgeComp.FactShipment ADD CONSTRAINT
   FK_FudgeComp_FactShipment_ShippedDateKey FOREIGN KEY
   (
   ShippedDateKey
   ) REFERENCES FudgeComp.DimDate
   ( DateKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
/*ALTER TABLE FudgeComp.FactRisk ADD CONSTRAINT
   FK_FudgeComp_FactRisk_OrderKey FOREIGN KEY
   (
   OrderKey
   ) REFERENCES FudgeComp.DimOrders
   ( OrderKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
*/
ALTER TABLE FudgeComp.FactRisk ADD CONSTRAINT
   FK_FudgeComp_FactRisk_CreditCardKey FOREIGN KEY
   (
   CreditCardKey
   ) REFERENCES FudgeComp.DimCreditCardInfo
   ( CreditCardKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
ALTER TABLE FudgeComp.FactRisk ADD CONSTRAINT
   FK_FudgeComp_FactRisk_CustomerKey FOREIGN KEY
   (
   CustomerKey
   ) REFERENCES FudgeComp.DimCustomers
   ( CustomerKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
ALTER TABLE FudgeComp.FactRisk ADD CONSTRAINT
   FK_FudgeComp_FactRisk_OrderDateKey FOREIGN KEY
   (
   OrderDateKey
   ) REFERENCES FudgeComp.DimDate
   ( DateKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
ALTER TABLE FudgeComp.FactRisk ADD CONSTRAINT
   FK_FudgeComp_FactRisk_ExpirationDateKey FOREIGN KEY
   (
   ExpirationDateKey
   ) REFERENCES FudgeComp.DimDate
   ( DateKey )
     ON UPDATE  NO ACTION
     ON DELETE  NO ACTION
;
 
