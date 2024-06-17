
sql_query -> "CREATE TABLE project AS
WITH bmi AS (
  SELECT
    o.subject_id,
    MAX(CAST(o.result_value AS FLOAT)) AS bmi
  FROM
    omr o
  WHERE
    o.result_name = 'BMI (kg/m2)'
    AND o.result_value IS NOT NULL
  GROUP BY o.subject_id
),
lab_items AS (
  SELECT
    ce.subject_id,
    ce.hadm_id,
    ce.stay_id,
    MAX(CASE WHEN di.label = 'Heart Rate' THEN ce.valuenum ELSE NULL END) AS heart_rate,
    MAX(CASE WHEN di.label = 'WBC' THEN ce.valuenum ELSE NULL END) AS wbc
  FROM
    chartevents ce
  JOIN
    d_items di ON ce.itemid = di.itemid
  WHERE
    di.label IN ('Heart Rate', 'WBC')
    AND ce.valuenum IS NOT NULL
  GROUP BY
    ce.subject_id, ce.hadm_id, ce.stay_id
)
SELECT 
  p.subject_id, 
  p.gender, 
  p.anchor_age,
  p.anchor_year,
  a.race,
  a.hadm_id,
  a.admittime,
  a.dischtime,
  a.hospital_expire_flag,
  i.stay_id,
  i.intime,
  i.outtime,
  MAX(CASE WHEN d.icd_code LIKE '401%' OR d.icd_code = 'I10' THEN 1 ELSE 0 END) AS hypertension,
  MAX(CASE WHEN d.icd_code LIKE '585%' OR d.icd_code LIKE 'N18%' THEN 1 ELSE 0 END) AS chronic_kidney_disease,
  MAX(CASE WHEN (d.icd_code LIKE 'C%' OR (d.icd_code BETWEEN 'C00' AND 'C96')) THEN 1 ELSE 0 END) AS cancer,
  b.bmi,
  l.heart_rate,
  l.wbc
FROM 
  patients p
JOIN 
  admissions a ON p.subject_id = a.subject_id
JOIN 
  icustays i ON a.hadm_id = i.hadm_id
JOIN 
  diagnoses_icd d ON i.hadm_id = d.hadm_id
LEFT JOIN 
  bmi b ON p.subject_id = b.subject_id
LEFT JOIN
  lab_items l ON p.subject_id = l.subject_id AND a.hadm_id = l.hadm_id AND i.stay_id = l.stay_id
GROUP BY 
  p.subject_id, p.gender, p.anchor_age, p.anchor_year, a.race, a.hadm_id, a.admittime, a.dischtime, a.hospital_expire_flag, i.stay_id, i.intime, i.outtime, b.bmi, l.heart_rate, l.wbc;
"











