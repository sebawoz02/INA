--1. 	
	CREATE DATABASE db_aparaty;
	CREATE USER 'index'@localhost;
	SET PASSWORD FOR 'index'@localhost=PASSWORD('SebastianL3D');
	GRANT SELECT, INSERT, UPDATE ON db_aparaty.* TO 'index'@localhost;

--2.	
	CREATE TABLE Aparat (model varchar(30) NOT NULL PRIMARY KEY,
	producent int,
	matryca int,
	obiektyw int,
	typ enum('kompaktowy','lustrzanka','profesjonalny','inny'),
	FOREIGN KEY(producent) REFERENCES Producent(ID) ON DELETE CASCADE,
	FOREIGN KEY(matryca) REFERENCES Matryca(ID) ON DELETE CASCADE,
	FOREIGN KEY(obiektyw) REFERENCES Obiektyw(ID) ON DELETE CASCADE
	);

	CREATE TABLE Matryca (ID int NOT NULL AUTO_INCREMENT,
	przekatna decimal(4,2) CHECK(przekatna>=0),
	rozdzielczosc decimal(3,1) CHECK(rozdzielczosc>=0),
	typ varchar(10),
	PRIMARY KEY(ID)
	)AUTO_INCREMENT=100;

	CREATE TABLE Obiektyw(ID int NOT NULL AUTO_INCREMENT,
	model varchar(30),
	minPrzeslona float CHECK(minPrzeslona>=0),
	maxPrzeslona float CHECK(maxPrzeslona>=0),
	PRIMARY KEY(ID),
	CONSTRAINT PrzeslonaCST CHECK(minPrzeslona<maxPrzeslona)
	);

	CREATE TABLE Producent(ID int NOT NULL AUTO_INCREMENT,
	nazwa varchar(50),
	kraj varchar(20),
	PRIMARY KEY(ID)
	);

--3.	
	mysql -u index -p
	
	INSERT INTO Producent (nazwa, kraj) VALUES
	( 'Agfa' , 'Chiny'), ( 'Casio', 'Niemcy'),
	( 'Canon', 'Polska'), ( 'Fujifilm', 'Chiny'),
	( 'Hewlet', 'Chiny'), ( 'Packard', 'Norwegia'),
	( 'Kodak', 'Czechy'), ('Leica', 'Chiny'),
	( 'Nikon', 'Japonia'), ('Olympus', 'Grecja'),
	( 'Panasonic', 'Chiny'), ('Pentax', 'USA'),
	( 'Ricoh', 'Anglia'), ('Samsung', 'Anglia'),
	( 'Sony', 'Niemcy');

	INSERT INTO Obiektyw (model, minPrzeslona,maxPrzeslona) VALUES
	('RF100', 2 , 8),('RF200',2,12),('TFx2',1,10),('TFx33',3,13),
	('PROf1', 2 , 13),('PROf2',2,15),('PROf3',1,16),('PH4K',4,11),
	('PH8K',5,20),('MODv1',3,7),('MODv2',3,10),('MODv3',2, 14),
	('OB1v1',1,11),('OB1v2',1,12),('OB1v3',1,16);
	('k',9,2),('l',-1,2);	--niepoprawne wartosci

	INSERT INTO Matryca (przekatna, rozdzielczosc,typ) VALUES
	(12.30, 30.3 ,'IPS'),(3.30, 12.3 ,'TN'),(77.88, 33.9 ,'VA'),
	(12.42, 86.4 ,'IPS'),(13.32, 33.5 ,'TN'),(45.75, 23.3 ,'VA'),
	(45.24, 34.1 ,'IPS'),(43.34, 22.1 ,'TN'),(34.75, 43.3 ,'VA'),
	(35.66, 12.3 ,'IPS'),(99.99, 03.2 ,'TN'),(11.66, 65.1 ,'VA'),
	(11.55, 90.4 ,'IPS'),(46.42, 11.1 ,'TN'),(11.35, 67.3 ,'VA');
	(-3, 2, 'kis'), (33, -33, 'kas'); --niepoprawne wartosci

	INSERT INTO Aparat (model, producent, matryca, obiektyw, typ) VALUES
	('A600', 1, 101 , 2, 'lustrzanka'),('A800', 1, 106 , 8, 'lustrzanka'),
	('B4400', 12, 100 , 6, 'inny'),('B5600', 12, 110 , 15, 'inny'),
	('KP-100', 5, 101 , 2, 'kompaktowy'),('KP-250', 5, 111 , 11, 'kompaktowy'),
	('PROK', 6, 103 , 4, 'profesjonalny'),('SaP00', 6, 108 , 12, 'profesjonalny'),
	('APARAT0', 8, 100 , 6, 'inny'),('APARAT1', 9, 112 , 9, 'inny'),
	('ART1000', 10, 104 , 5, 'kompaktowy'),('ART2000', 11, 102 , 3, 'kompaktowy'),
	('MOD1', 13, 114 , 12, 'inny'),('MOD22', 14, 107 , 14, 'profesjonalny'),
	('KART33', 2, 110 , 4, 'lustrzanka');

--4. Z konta index nie mozna stworzyc procedury.

	DELIMITER $$
	CREATE FUNCTION fn_generate_random_code (desired_code_len INTEGER) RETURNS VARCHAR(100)
	NO SQL
	BEGIN
		SET @possible_characters = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
		SET @len =  LENGTH(@possible_characters);
    		SET @random_code = '';
		append_char_to_random_code: LOOP
			IF LENGTH(@random_code) >= desired_code_len THEN
			LEAVE append_char_to_random_code;
		END IF;
		SET @random_char_pos = FLOOR(RAND()*(@len - 0 + 1) + 0);
		SET @extracted_char = SUBSTRING(@possible_characters, @random_char_pos, 1);
		SET @random_code = CONCAT(@random_code, @extracted_char);
	END LOOP;
	RETURN @random_code;
	END $$
	DELIMITER ;

	
	DELIMITER $$
	CREATE PROCEDURE InsertRandomRecords ()
	BEGIN
		DECLARE cnt INT;
		SET cnt=0;
		WHILE cnt < 100 DO
		IF cnt % 4 = 0 THEN
			SET @typ = 'inny';
		END IF;
		IF cnt % 4 = 1 THEN
			SET @typ = 'lustrzanka';
		END IF;
		IF cnt % 4 = 2 THEN
			SET @typ = 'profesjonalny';
		END IF;
		IF cnt % 4 = 3 THEN
			SET @typ = 'kompaktowy';
		END IF;
   		INSERT INTO Aparat (model, producent, matryca, obiektyw, typ) VALUES
		((fn_generate_random_code(6)),
		(SELECT ID FROM Producent ORDER BY RAND() LIMIT 1),
		(SELECT ID FROM Matryca ORDER BY RAND() LIMIT 1),
		(SELECT ID FROM Obiektyw ORDER BY RAND() LIMIT 1),
		(@typ)
		);
   		SET cnt = cnt + 1;
		END WHILE;
	END $$
	DELIMITER ;

--5.
	DELIMITER $$
	CREATE FUNCTION max_diagonal ( producent_id INTEGER) RETURNS VARCHAR(30)
	NO SQL
	BEGIN
	SET @model = (SELECT model FROM Aparat AS a JOIN Matryca AS m ON a.matryca=m.ID WHERE a.producent=producent_id ORDER BY przekatna LIMIT 1);
	RETURN @model;
	END $$
	DELIMITER ;

--6.
	DELIMITER $$
	CREATE TRIGGER add_model BEFORE INSERT ON Aparat
	FOR EACH ROW
	BEGIN 
		IF NEW.producent NOT IN (SELECT ID FROM Producent) THEN
			INSERT INTO Producent(ID) VALUES (NEW.producent);
		END IF;
	END $$
	DELIMITER ;

--7.
	DELIMITER $$
	CREATE FUNCTION num_of_cameras ( matryca_id INTEGER) RETURNS INTEGER
	NO SQL
	BEGIN
		SET @num = (SELECT COUNT(model) FROM Aparat WHERE matryca=matryca_id);
	RETURN @num;
	END $$
	DELIMITER ;

--8.
	DELIMITER $$
	CREATE TRIGGER delete_matryca AFTER DELETE ON Aparat
	FOR EACH ROW
	BEGIN 
		IF OLD.matryca NOT IN(SELECT matryca FROM Aparat) THEN
			DELETE FROM Matryca WHERE ID=OLD.matryca; 
		END IF;
	END $$
	DELIMITER ;
	
--9.	Z konta index nie mozna stworzyc widoku.

	CREATE VIEW widok_zad9 AS(SELECT a.model, p.nazwa, m.przekatna, m.rozdzielczosc, o.minPrzeslona, o.maxPrzeslona FROM Aparat a
	JOIN Matryca m ON a.matryca=m.ID JOIN Obiektyw o ON a.obiektyw=o.ID JOIN Producent p ON a.producent=p.ID
	WHERE a.typ='lustrzanka' AND p.kraj NOT LIKE 'Chiny');

--10.
	CREATE VIEW widok_zad10 AS(SELECT p.nazwa, p.kraj, a.model FROM Producent p JOIN Aparat a ON p.ID=a.producent);

	DELETE FROM Aparat WHERE producent IN(SELECT ID FROM Producent WHERE kraj='Chiny');

-- Z widoku zniknely modele z chinskim producentem.

--11.	Z konta index nie mozna stworzyc triggerow.

	ALTER TABLE Producent ADD liczbaModeli INTEGER NOT NULL;

	DELIMITER $$
	CREATE PROCEDURE updateLiczbaModeli()
	BEGIN
		DECLARE cnt INT;
		SET cnt = 0;
		WHILE cnt < 16 DO
			UPDATE Producent SET liczbaModeli=(SELECT COUNT(model) FROM Aparat WHERE producent=cnt) WHERE ID = cnt;
			SET cnt = cnt +1;
		END WHILE;
		UPDATE Producent SET liczbaModeli=(SELECT COUNT(model) FROM Aparat WHERE producent=20) WHERE ID = 20;
	END $$
	DELIMITER ;
	
	DELIMITER $$
	CREATE TRIGGER triggerLiczbaModeliIns AFTER INSERT ON Aparat
	FOR EACH ROW
	BEGIN 
		CALL updateLiczbaModeli();
	END $$
	DELIMITER ;

	DELIMITER $$
	CREATE TRIGGER triggerLiczbaModeliDel AFTER DELETE ON Aparat
	FOR EACH ROW
	BEGIN 
		CALL updateLiczbaModeli();
	END $$
	DELIMITER ;

	DELIMITER $$
	CREATE TRIGGER triggerLiczbaModeliUpd AFTER UPDATE ON Aparat
	FOR EACH ROW
	BEGIN 
		CALL updateLiczbaModeli();
	END $$
	DELIMITER ;
