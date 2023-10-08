--1.
SHOW FULL TABLES;

--2.
SELECT title FROM film WHERE length>120;

--3.
SELECT title FROM film WHERE rating LIKE 'G' ORDER BY length LIMIT 4;

--4.
SELECT f.title,l.name FROM film AS f JOIN language AS l ON f.language_id = l.language_id 
WHERE description LIKE '%Drama%';

--5
SELECT f.title FROM film AS f
  JOIN film_category AS fc ON f.film_id = fc.film_id
  JOIN category AS c ON fc.category_id = c.category_id
  WHERE c.name='Family' AND f.description LIKE '%Documentary%';

--6.
SELECT f.title FROM film AS f
JOIN film_category AS fc ON f.film_id = fc.film_id
JOIN category AS c ON fc.category_id = c.category_id
WHERE c.name='Children' AND rating NOT LIKE 'G';

--7.
SELECT rating, COUNT(film_id) FROM film GROUP BY rating;

--8.
SELECT DISTINCT f.title FROM film AS f
JOIN inventory AS inv ON f.film_id=inv.film_id
JOIN rental AS r ON inv.inventory_id=r.inventory_id
WHERE r.rental_date>'2005-05-31' AND r.rental_date<'2005-06-15'
ORDER BY f.title ASC;

--9.
SELECT DISTINCT first_name, last_name FROM actor AS a
JOIN film_actor AS fa ON a.actor_id=fa.actor_id
JOIN film AS f ON fa.film_id=f.film_id
WHERE f.special_features LIKE '%Deleted Scenes%';

--10.
SELECT c.first_name, c.last_name FROM customer AS c
JOIN payment AS p ON c.customer_id=p.customer_id
JOIN rental AS r ON p.rental_id=r.rental_id
WHERE p.staff_id<>r.staff_id;

--OR

CREATE VIEW difStaf AS(SELECT p.customer_id FROM payment p 
JOIN rental r ON p.rental_id=r.rental_id WHERE p.staff_id<>r.staff_id);

SELECT first_name,last_name FROM customer WHERE customer_id IN (SELECT * FROM difStaf);

--11.
SELECT c.first_name,c.last_name FROM customer c JOIN rental r
ON c.customer_id=r.customer_id GROUP BY c.first_name, c.last_name 
HAVING COUNT(r.rental_id)>(SELECT COUNT(r.rental_id) FROM rental r
JOIN customer c ON r.customer_id=c.customer_id WHERE c.email='MARY.SMITH@sakilacustomer.org');

--12.
SELECT A.actor_id, B.actor_id FROM film_actor A, film_actor B WHERE
A.actor_id<B.actor_id AND A.film_id=B.film_id GROUP BY A.actor_id,B.actor_id HAVING COUNT(A.actor_id)>1;

--13.
SELECT last_name FROM actor
WHERE actor_id NOT IN(SELECT a.actor_id FROM actor AS a
JOIN film_actor AS fa ON a.actor_id=fa.actor_id
JOIN film AS f ON fa.film_id=f.film_id
 WHERE f.title LIKE 'C%');

--14.  
CREATE VIEW horror_films AS(SELECT a.actor_id AS aid,a.last_name AS lname,COUNT(fa.film_id) AS h_num FROM actor a
JOIN film_actor fa ON a.actor_id=fa.actor_id JOIN film f ON fa.film_id=f.film_id
JOIN film_category fc ON f.film_id=fc.film_id JOIN category c ON fc.category_id=c.category_id
WHERE c.name='Horror' GROUP BY aid);

CREATE VIEW action_films AS(SELECT a.actor_id AS aid,a.last_name AS lname,COUNT(fa.film_id)AS a_num FROM actor a
JOIN film_actor fa ON a.actor_id=fa.actor_id JOIN film f ON fa.film_id=f.film_id
WHERE f.title IN(SELECT f.title FROM film f JOIN film_category fc ON f.film_id=fc.film_id
JOIN category c ON fc.category_id=c.category_id WHERE c.name='Action') GROUP BY aid);

SELECT a.lname FROM horror_films a
JOIN action_films b ON a.aid=b.aid
WHERE a.h_num<b.a_num;

--15. 
SELECT c.customer_id FROM customer c JOIN payment p  ON c.customer_id=p.customer_id GROUP BY c.customer_id
HAVING AVG(p.amount)<(SELECT AVG(amount) FROM payment 
WHERE payment_date BETWEEN '2005-07-30 00:00:00' AND '2005-07-30 23:59:59');

--16. 
UPDATE film SET language_id=(SELECT language_id FROM language WHERE name='Italian') WHERE title='YOUNG LANGUAGE';

--17.	
INSERT INTO language(language_id,name) VALUES (7,'Spanish');
UPDATE film f JOIN film_actor fa ON f.film_id=fa.film_id JOIN actor a ON fa.actor_id=a.actor_id  SET language_id=7
WHERE a.first_name='ED' AND a.last_name='CHASE';

--18.
ALTER TABLE language ADD films_no bigint;
UPDATE language SET films_no=
(SELECT COUNT(*) FROM film f WHERE f.language_id=language.language_id);
   
--19.
ALTER TABLE film DROP COLUMN release_year;
