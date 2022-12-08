# SDM_FinalProject_GarmentEmployee-Productivity

Collaborators : 

Jitesh 
Mahimitra Chirala
Sai kiran Paturi


One of the most important cases of industrial globalization in this day and age is the Garment Industry. It is a field that uses a lot of manual processes and requires a lot of labor. The performance of the workers in the garment manufacturing enterprises in terms of production and delivery is largely responsible for meeting the enormous global demand for clothing products. Decision-makers in the apparel business therefore find it highly desirable to monitor, evaluate, and forecast the productivity performance of the working teams in their factories.

The dataset contains significant characteristics of the garment manufacturing process and employee productivity that were personally collected and verified by professionals in the field.

Our main goal is to determine whether there is a way to increase the productivity of garment workers. We think that using the well-known statistical technique known as regression would enable us to accomplish the goal. Why only Regression? Why not Classification? We do not take classification into account because productivity is a measure that is determined by taking several factors into account. If productivity were to be classified into, say, two or three classes, this could result in inaccurate predictions that would have an adverse effect on the improvement of productivity.

The dataset contains 15 attributes and 1197 entries. When analyzing each column, it appears that the columns "Quarter," "Date," and "Day" are connected. Date and Day are represented as a quarter in a generic sense. Thus, eliminating the Date and Day columns might not affect the predictions. The handling of null values comes next. The 506 null values in the "wip" column are replaced with "0" because it is a numeric attribute.
We discovered several outliers in the data after further analysis, therefore we managed the outliers by using Inter Quantile Range and filtering the impacted variables with the first and third quantiles.
Next, nominal attributes are subjected to label encoding.
Finally, Each attribute in the data is then scaled (with a mean of 0 and a variance of 1). And the train_test split is obtained from those.
