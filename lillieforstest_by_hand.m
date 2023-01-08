function [my_h, my_KS, crit_value] = lillieforstest(data, alpha)
logreturns_data = log(data(2:end,:))-log(data(1:end-1,:));
n = size(logreturns_data, 1);
if alpha == 0.05 && n > 50
    crit_value = 0.895/((0.83+n)/sqrt(n)-0.01);
elseif alpha == 0.01 && n > 50
    crit_value = 1.035/((0.83+n)/sqrt(n)-0.01);
end
x = linspace(min(logreturns_data), max(logreturns_data), n);
theory_cdf = normcdf(x, mean(logreturns_data), std(logreturns_data));
my_ecdf = zeros(1, n); 
sorted_logreturns = sort(logreturns_data);
for i=1:n 
    yi = x(i);
    my_ecdf(i) = sum(sorted_logreturns <= yi)/n;
end
my_KS = max(abs(my_ecdf-theory_cdf));
plot(x,theory_cdf,'LineWidth',2,'Color','blue'); %wykresy 
hold on
plot(x,my_ecdf,'LineWidth',2,'Color','red')
xlabel('return');
ylabel('CDF');
leg=legend({'Normal CDF','Empirical CDF'});
axis([-0.15 0.15 0 1]);
set(gca,'FontSize',15);
legend boxoff 
hold off;
fprintf('%29s %5.4f \n', "Lilliefors test statistic is:", my_KS); 
if my_KS > crit_value
    my_h = 1;
    fprintf('%17s %5.4f %25s \n', "Critical value is", crit_value,", thus we reject the H_0.")
else
    my_h = 0; 
    fprintf('%17s %5.4f %25s \n', "Critical value is", crit_value,", thus we failed to reject the H_0.")
end
end