--drop procedures
IF OBJECT_ID('dbo.AddNewOrder') IS NOT NULL
	DROP PROCEDURE dbo.AddNewOrder
GO
IF OBJECT_ID('dbo.UpdateOrder') IS NOT NULL
	DROP PROCEDURE dbo.UpdateOrder
GO
IF OBJECT_ID('dbo.SearchBarCode') IS NOT NULL
	DROP PROCEDURE dbo.SearchBarCode
GO
IF OBJECT_ID('dbo.SalesHistoryForDepartment') IS NOT NULL
	DROP PROCEDURE dbo.SalesHistoryForDepartment
GO
IF OBJECT_ID('dbo.GetItemAverageShelfLife') IS NOT NULL
	DROP PROCEDURE dbo.GetItemAverageShelfLife
GO
IF OBJECT_ID('dbo.GetProfitsByMonth') IS NOT NULL
	DROP PROCEDURE dbo.GetProfitsByMonth
GO
--drop Types
IF TYPE_ID(N'ItemsListType') IS NOT NULL
	DROP TYPE dbo.ItemsListType
GO
--drop views
IF OBJECT_ID('dbo.ItemsToProcureBasedOnOrders') IS NOT NULL
	DROP VIEW dbo.ItemsToProcureBasedOnOrders
GO
IF OBJECT_ID('dbo.ProfitPerItem') IS NOT NULL
	DROP VIEW dbo.ProfitPerItem
GO
IF OBJECT_ID('dbo.ProcurementStats') IS NOT NULL
	DROP VIEW dbo.ProcurementStats
GO
IF OBJECT_ID('dbo.CurrentSalesPerDepartment') IS NOT NULL
	DROP VIEW dbo.CurrentSalesPerDepartment
GO
IF OBJECT_ID('dbo.SalesBasedOnItems') IS NOT NULL
	DROP VIEW dbo.SalesBasedOnItems
GO

--View Creation
CREATE VIEW ItemsToProcureBasedOnOrders
AS
SELECT items.Name, SUM(OrderItems.Quantity) AS [Ordered], items.Quantity AS [Remaining]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS OrderItems
	ON orders.OrderID = OrderItems.OrderID
INNER JOIN prj_Item AS items
	ON OrderItems.ItemID = items.ItemID
WHERE items.Quantity <= 15
AND orders.OrderDate BETWEEN DATEADD(DAY, -31, GETDATE()) AND  GETDATE()
GROUP BY items.Name, items.Quantity
GO

CREATE VIEW SalesBasedOnItems
AS
SELECT items.Name, COUNT(1) AS [Ordered]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS OrderItems
	ON orders.OrderID = OrderItems.OrderID
INNER JOIN prj_Item AS items
	ON OrderItems.ItemID = items.ItemID
WHERE orders.OrderDate BETWEEN DATEADD(DAY, -31, GETDATE()) AND  GETDATE()
GROUP BY items.Name
GO

CREATE VIEW ProfitPerItem
AS
SELECT 
	department.Name AS [Department_Name],
	item.Name AS [Item_Name], 
	item.Description AS [Item_Description],
	item.Cost AS [Purchase_Price],
	item.Price AS [Sale_Price],
	CONVERT(VARCHAR, CONVERT(INT,ROUND(((item.Price - item.Cost)/item.Cost)*100, 0))) + '%' AS [Margin],
	SUM(orderItem.Quantity) AS [Quantity_Sold],
	item.Quantity AS [Quantity_Remaining],
	SUM((orderItem.Quantity * (item.Price - (item.Price * ISNULL(orderItem.DiscountPercentage, 0)/100))) - (orderItem.Quantity * item.Cost)) AS [Profit],
	CONVERT(VARCHAR, CONVERT(INT,ROUND(SUM((orderItem.Quantity * (item.Price - (item.Price * ISNULL(orderItem.DiscountPercentage, 0)/100))) - (orderItem.Quantity * item.Cost))/SUM(orderItem.Quantity * item.Price)*100, 0))) + '%' AS [Profit_Percent]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS orderItem
	ON orders.OrderID = orderItem.OrderID
INNER JOIN prj_Item AS item
	ON orderItem.ItemID = item.ItemID
INNER JOIN prj_Department AS department
	ON item.DepartmentID = department.DepartmentID
GROUP BY department.Name,
		item.Name, 
		item.Description,
		item.Cost,
		item.Price,
		item.Quantity,
		CONVERT(VARCHAR, (item.Price - item.Cost)/item.Cost)
GO

CREATE VIEW ProcurementStats
AS
SELECT vendor.FirstName AS [vendor_First_name],
		vendor.LastName AS [vendor_last_name],
		item.Name AS [Item_Name],
		AVG(DATEDIFF(DAY, orders.OrderDate, orders.OrderFullfilmentDate)) AS [days_needed]
FROM prj_ProcurementOrders AS orders
INNER JOIN prj_ProcurementOrderList AS orderList
	ON orders.OrderID = orderList.OrderID
INNER JOIN prj_Item AS item
	ON orderList.ItemID = item.ItemID
INNER JOIN prj_Vendor AS vendor
	ON orders.VendorID = vendor.VendorID
WHERE orders.OrderFullfilmentDate IS NOT NULL
GROUP BY vendor.FirstName,
		vendor.LastName,
		item.Name
GO

CREATE VIEW CurrentSalesPerDepartment
AS
SELECT departments.Name,
		SUM((orderItems.Quantity * (items.Price - (items.Price * ISNULL(OrderItems.DiscountPercentage, 0) / 100))) - (OrderItems.Quantity * items.Cost)) AS [Profit]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS OrderItems
	ON orders.OrderID = OrderItems.OrderID
INNER JOIN prj_Item AS items
	ON OrderItems.ItemID = items.ItemID
INNER JOIN prj_Department AS departments
	On items.DepartmentID = departments.DepartmentID
WHERE YEAR(orders.OrderDate) = YEAR(GETDATE())
	AND MONTH(orders.OrderDate) = MONTH(GETDATE())
GROUP BY departments.Name
GO

--Type creation
CREATE TYPE ItemsListType AS Table (itemId INT, quantity INT, discount INT)  
GO

--Procedure creation
CREATE PROCEDURE GetProfitsByMonth
AS
BEGIN
SELECT YEAR(orders.OrderDate) AS [Year],
		MONTH(orders.OrderDate) AS [Month],
		SUM((orderItems.Quantity * (items.Price - (items.Price * ISNULL(OrderItems.DiscountPercentage, 0) / 100))) - (OrderItems.Quantity * items.Cost)) AS [Profit]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS OrderItems
	ON orders.OrderID = OrderItems.OrderID
INNER JOIN prj_Item AS items
	ON OrderItems.ItemID = items.ItemID
GROUP BY YEAR(orders.OrderDate),
		MONTH(orders.OrderDate)
ORDER BY [Year] DESC, [Month] DESC
END
GO

CREATE PROCEDURE AddNewOrder(
@p_OrderNumber VARCHAR(20),
@p_PaymentAmount DECIMAL,
@p_paymentModeId INT,
@p_orderTypeId INT,
@p_customerId INT,
@p_items ItemsListType READONLY) AS
BEGIN
INSERT INTO prj_Order (ordernumber, orderdate, orderstatus, paymentamount, paymentmodeid, ordertypeid, customerid)
VALUES (@p_OrderNumber, GETDATE(), 'Completed', @p_PaymentAmount, @p_paymentModeId, @p_orderTypeId, @p_customerId)

DECLARE @orderID INT = @@IDENTITY

INSERT INTO prj_OrderItemList(orderid, itemid, quantity, DiscountPercentage)
SELECT @orderID, itemId, quantity, discount
FROM @p_items
END
GO

CREATE PROCEDURE UpdateOrder(
@p_orderNumber VARCHAR(20),
@p_OrderStatus VARCHAR(10)) AS
BEGIN

UPDATE prj_Order
SET OrderStatus = @p_OrderStatus
WHERE OrderNumber = @p_orderNumber

END
GO

CREATE PROCEDURE SearchBarCode(
@p_BarCode VARCHAR(20)) AS
SELECT [Name] AS Item_Name,
		Brand AS Item_Brand,
		[Description] AS [Item_description],
		Quantity AS [Remaining_Quantity],
		Price AS [Item_Price]
FROM prj_Item WHERE Barcode = @p_BarCode
GO

CREATE PROCEDURE SalesHistoryForDepartment (
@p_DepartmentId INT) AS
SELECT departments.Name,
		YEAR(orders.OrderDate) AS [Year],
		MONTH(orders.OrderDate) AS [Month],
		SUM((orderItems.Quantity * (items.Price - (items.Price * ISNULL(OrderItems.DiscountPercentage, 0) / 100))) - (OrderItems.Quantity * items.Cost)) AS [Profit]
FROM prj_Order AS orders
INNER JOIN prj_OrderItemList AS OrderItems
	ON orders.OrderID = OrderItems.OrderID
INNER JOIN prj_Item AS items
	ON OrderItems.ItemID = items.ItemID
INNER JOIN prj_Department AS departments
	On items.DepartmentID = departments.DepartmentID
WHERE departments.DepartmentID = @p_DepartmentId
GROUP BY departments.Name,
		YEAR(orders.OrderDate),
		MONTH(orders.OrderDate)
ORDER BY [Year] DESC, [Month] DESC
GO

CREATE PROCEDURE GetItemAverageShelfLife AS
BEGIN
	DECLARE @itemOrders TABLE(
	item_Name VARCHAR(50),
	shelf_life INT)

	INSERT INTO @itemOrders
	SELECT items.Name,
			DATEDIFF(DAY, orders.OrderDate, LEAD(orders.OrderDate, 1) OVER(PARTITION BY items.Name ORDER BY orders.OrderDate)) [shelf_life]
	FROM prj_ProcurementOrders AS orders
	INNER JOIN prj_ProcurementOrderList AS orderList
		ON orders.OrderID = orderList.OrderID
	INNER JOIN prj_Item AS items
		ON orderList.ItemID = items.ItemID
		ORDER BY items.Name

	SELECT item_name, ISNULL(AVG(shelf_life), 999) AS [shelf_life]
	FROM @itemOrders
	GROUP BY item_Name
	ORDER BY shelf_life ASC
END