#!bin/sh

train_size=10
validate_size=10
num_features=20
true_num_groups=1
expert_num_groups=4
num_runs=5

echo $expert_num_groups > results/expert_num_groups
python gen_data.py $train_size $validate_size $num_features $true_num_groups $num_runs
echo "Success!"
