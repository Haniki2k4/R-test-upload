Dữ liệu phân tích được trích xuất từ hệ thống mã hóa ICD-10 tại cơ sở y tế, bao gồm 3 bảng chính: admission, diagnosis, và ICD10. Dưới góc nhìn của một nhà khoa học dữ liệu, nhóm tiến hành đánh giá chất lượng số liệu theo các khía cạnh sau:

**1. 🧩 Tính đầy đủ (Completeness)**

-   Đa số trường hợp nhập viện đều có mã chẩn đoán chính (Other = P) và ít nhất một mã ICD-10 (V01-V99) cho tai nạn giao thông.

-   Tuy nhiên, còn thiếu thông tin trong một số trường hợp như là một số mã ICD không có mô tả (Unknow, NA).

-   Dựa trên góc độ là **nhà khoa học dữ liệu** thì cần thêm 1 số thông tin để ra quyết định như:

    -   Thời gian nhập viện / ngày xảy ra tai nạn.

    -   Địa phương hoặc nơi xảy ra tai nạn

    -   Phương tiện tham gia giao thông

    -   Biến số hành vi nguy cơ (mũ bảo hiểm, đi sai luật,...)

**2. 🎯 Tính chính xác (Accuracy)**

⚠ Không thể xác định 100% mã ICD-10 có chính xác với chẩn đoán lâm sàng do thiếu bản ghi bệnh án gốc để đối chiếu.

Tuy nhiên, hệ thống đã sử dụng phương pháp:

-   Lọc đúng mã ICD-10 dạng TNGT (V01-V99).

-   Ánh xạ chẩn đoán kèm theo bằng quy tắc ICD (S06, S72, S50...).

**3. 🔁 Tính nhất quán (Consistency)**

✅ Trong một bệnh nhân (admission_id), các biến tuổi (age_years), giới tính (sex), thời gian nằm viện (los), loại hình chăm sóc(caretype) tương đối thống nhất.

⚠ Có một số trường hợp mã kí tự nguồn nhập viện (admtype) có những dấu hiệu không khớp khi sử dụng mô tả các kí tự.

**4. 🔍 Tính chi tiết (Specificity/Granularity)**

✅ Gần như toàn bộ các mã ICD-10 thuộc nhóm tai nạn giao thông đều được mã hóa ở cấp **4 ký tự đầy đủ**, ví dụ: `V031`, `V123`, `V489`... Điều này đảm bảo đủ mức độ chi tiết để phân tích sâu theo loại phương tiện, vị trí xảy ra tai nạn, v.v.

**5. ⏱️ Tính kịp thời và khả dụng (Timeliness & Availability)**

✅ Dữ liệu sẵn sàng để phân tích và đủ phục vụ mục tiêu đề tài.

⚠ Tuy nhiên thiếu các biến thời gian như ngày nhập viện hoặc thời điểm xảy ra tai nạn gây hạn chế cho việc phân tích xu hướng theo mùa vụ.

***✅ Kết luận***

*Tập dữ liệu có chất lượng ở mức đủ để phân tích mô tả và thống kê cơ bản cho đề tài. Tuy nhiên, để ứng dụng tốt hơn trong nghiên cứu sâu hoặc mô hình dự đoán, cần cải thiện các vấn đề về:*

*Chi tiết mã hóa (cụ thể hóa hơn)*

*Đầy đủ thông tin (LOS, ngày tháng, nguyên nhân rõ ràng)*

*Tính xác thực (so sánh với hồ sơ lâm sàng khi có)*
