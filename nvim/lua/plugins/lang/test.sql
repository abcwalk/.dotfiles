-- surname | salary
-- ----------------
-- john    | 100000|
-- lena    | 300000|
-- nastya  | 300000|
-- ----------------


SELECT surname, salary
FROM users
ORDER by salary desc
LIMIT 1;

SELECT surname, salary
FROM users
WHERE salary = (SELECT MAX(salary) FROM users);
