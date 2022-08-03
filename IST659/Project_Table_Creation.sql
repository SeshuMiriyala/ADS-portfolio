IF OBJECT_ID('dbo.prj_Employee') IS NOT NULL
	DROP TABLE dbo.prj_Employee
IF OBJECT_ID('dbo.prj_Role') IS NOT NULL
	DROP TABLE dbo.prj_Role
IF OBJECT_ID('prj_OrderItemList') IS NOT NULL
	DROP TABLE dbo.prj_OrderItemList
IF OBJECT_ID('prj_ProcurementOrderList') IS NOT NULL
	DROP TABLE dbo.prj_ProcurementOrderList
IF OBJECT_ID('dbo.prj_Item') IS NOT NULL
	DROP TABLE dbo.prj_Item
IF OBJECT_ID('dbo.prj_Department') IS NOT NULL
	DROP TABLE dbo.prj_Department
IF OBJECT_ID('dbo.prj_ProcurementOrders') IS NOT NULL
	DROP TABLE dbo.prj_ProcurementOrders
IF OBJECT_ID('dbo.prj_Vendor') IS NOT NULL
	DROP TABLE dbo.prj_Vendor
IF OBJECT_ID('prj_Order') IS NOT NULL
	DROP TABLE dbo.prj_Order
IF OBJECT_ID('dbo.prj_PaymentMode') IS NOT NULL
	DROP TABLE dbo.prj_PaymentMode
IF OBJECT_ID('dbo.prj_OrderType') IS NOT NULL
	DROP TABLE dbo.prj_OrderType
IF OBJECT_ID('dbo.prj_Customer') IS NOT NULL
	DROP TABLE dbo.prj_Customer

--Tables Creation
CREATE TABLE [dbo].[prj_Role](
	RoleID INT IDENTITY(1,1) PRIMARY KEY,
	Title VARCHAR(20) NOT NULL,
	[Description] VARCHAR(50),
	PayRate DECIMAL(5,2) NOT NULL,
	CONSTRAINT U1_prj_Role UNIQUE (Title)
)
GO

CREATE TABLE [dbo].[prj_Department](
	DepartmentID INT IDENTITY(1,1) PRIMARY KEY,
	[Name] VARCHAR(50) NOT NULL,
	CONSTRAINT U1_prj_Department UNIQUE ([Name])
)
GO

CREATE TABLE [dbo].[prj_Employee](
	[EmployeeID] [int] IDENTITY(1,1) PRIMARY KEY,
	FirstName VARCHAR(20),
	LastName VARCHAR(20),
	AddressLine1 VARCHAR(50),
	AddressLine2 VARCHAR(50),
	City VARCHAR(20),
	[State] VARCHAR(10),
	Zipcode VARCHAR(5),
	Phone VARCHAR(12),
	EmailAddress VARCHAR(50),
	DepartmentID INT NOT NULL,
	SuperVisorID INT,
	RoleID INT,
	CONSTRAINT U1_prj_Employee UNIQUE (FirstName, LastName),
	CONSTRAINT U2_prj_Employee UNIQUE (Phone),
	CONSTRAINT U3_prj_Employee UNIQUE (EmailAddress),
	CONSTRAINT FK1_prj_Employee FOREIGN KEY (DepartmentID) REFERENCES prj_Department(DepartmentID),
	CONSTRAINT FK2_prj_Employee FOREIGN KEY (SupervisorID) REFERENCES prj_Employee (EmployeeID),
	CONSTRAINT FK3_prj_Employee FOREIGN KEY (RoleID) REFERENCES prj_Role(RoleID)
)
GO

CREATE TABLE [dbo].[prj_PaymentMode](
	PaymentModeID INT IDENTITY(1,1) PRIMARY KEY,
	[Name] VARCHAR(20) NOT NULL,
	[Provider] VARCHAR(20),
	[Description] VARCHAR(50),
	CONSTRAINT U1_prj_PaymentMode UNIQUE ([Name], [Provider])
)
GO

CREATE TABLE [dbo].[prj_OrderType](
	OrderTypeID INT IDENTITY(1,1) PRIMARY KEY,
	[Type] VARCHAR(20) NOT NULL,
	[Description] VARCHAR(50),
	CONSTRAINT U1_prj_OrderType UNIQUE ([Type])
)
GO

CREATE TABLE [dbo].[prj_Customer](
	CustomerID INT IDENTITY(1,1) PRIMARY KEY,
	FirstName VARCHAR(20) NOT NULL,
	LastName VARCHAR(20) NOT NULL,
	AddressLine1 VARCHAR(50),
	AddressLine2 VARCHAR(50),
	City VARCHAR(20),
	[State] VARCHAR(10),
	Zipcode VARCHAR(5),
	Phone VARCHAR(12) NOT NULL,
	EmailAddress VARCHAR(50) NOT NULL,
	CONSTRAINT U1_prj_Customer UNIQUE (FirstName, LastName),
	CONSTRAINT U2_prj_Customer UNIQUE (Phone),
	CONSTRAINT U3_prj_Customer UNIQUE (EmailAddress)
)
GO

CREATE TABLE [dbo].[prj_Vendor](
	VendorID INT IDENTITY(1,1) PRIMARY KEY,
	FirstName VARCHAR(20) NOT NULL,
	LastName VARCHAR(20) NOT NULL,
	CompanyName VARCHAR(100) NOT NULL,
	Phone VARCHAR(12) NOT NULL,
	EmailAddress VARCHAR(50) NOT NULL,
	CompanyAddressLine1 VARCHAR(50) NOT NULL,
	CompanyAddressLine2 VARCHAR(50),
	CompanyCity VARCHAR(20) NOT NULL,
	CompanyState VARCHAR(10) NOT NULL,
	CompanyZipcode VARCHAR(5) NOT NULL
)
GO

CREATE TABLE [dbo].[prj_Item](
	ItemID INT IDENTITY(1,1) PRIMARY KEY,
	[Name] VARCHAR(50) NOT NULL,
	Brand VARCHAR(100) NOT NULL,
	[Description] VARCHAR(100),
	Barcode VARCHAR(20) NOT NULL,
	Quantity INT,
	Price DECIMAL(5,2),
	Cost DECIMAL(5,2),
	DepartmentID INT NOT NULL,
	CONSTRAINT U1_prj_Item UNIQUE (Barcode),
	CONSTRAINT F1_prj_Item FOREIGN KEY (DepartmentID) REFERENCES prj_Department(DepartmentID)
)
GO

CREATE TABLE [dbo].[prj_ProcurementOrders](
	OrderID INT IDENTITY(1,1) PRIMARY KEY,
	OrderDate DateTime NOT NULL,
	Price DECIMAL(5,2) NOT NULL,
	VendorID INT NOT NULL,
	OrderFullfilmentDate DateTime,
	CONSTRAINT F1_prj_ProcurementOrders FOREIGN KEY (VendorID) REFERENCES prj_Vendor(VendorID)
)
GO

CREATE TABLE [dbo].[prj_ProcurementOrderList](
	ProcurementOrderListID INT IDENTITY(1,1) PRIMARY KEY,
	OrderID INT NOT NULL,
	ItemID INT NOT NULL,
	Quantity INT NOT NULL,
	CONSTRAINT FK1_prj_ProcurementOrderlist FOREIGN KEY (OrderID) REFERENCES prj_ProcurementOrders(OrderID),
	CONSTRAINT FK2_prj_ProcurementOrderList FOREIGN KEY (ItemID) REFERENCES prj_Item(ItemID)
)
GO

CREATE TABLE [dbo].[prj_Order](
	OrderID INT IDENTITY(1,1) PRIMARY KEY,
	OrderNumber VARCHAR(20) NOT NULL,
	OrderDate DateTime NOT NULL,
	OrderStatus VARCHAR(10) NOT NULL,
	PaymentAmount Decimal NOT NULL,
	PaymentModeID INT NOT NULL,
	OrderTypeID INT NOT NULL,
	CustomerID INT,
	CONSTRAINT U1_prj_Order UNIQUE (OrderNumber),
	CONSTRAINT FK1_prj_Order FOREIGN KEY (PaymentModeID) REFERENCES prj_PaymentMode(PaymentModeID),
	CONSTRAINT FK2_prj_Order FOREIGN KEY (OrderTypeID) REFERENCES prj_OrderType(OrderTypeID),
	CONSTRAINT FK3_prj_Order FOREIGN KEY (CustomerID) REFERENCES prj_Customer(CustomerID)
)
GO

CREATE TABLE [dbo].[prj_OrderItemList](
	OrderItemListID INT IDENTITY(1,1) PRIMARY KEY,
	OrderID INT NOT NULL,
	ItemID INT NOT NULL,
	Quantity INT NOT NULL,
	DiscountPercentage INT,
	CONSTRAINT FK1_prj_OrderItemList FOREIGN KEY (OrderID) REFERENCES prj_Order(OrderID),
	CONSTRAINT FK2_prj_OrderItemList FOREIGN KEY (ItemID) REFERENCES prj_Item(ItemID)
)
GO