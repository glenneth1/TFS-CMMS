// TF SAFE CMMS - Frontend JavaScript

document.addEventListener('DOMContentLoaded', function() {
  // Team number autocomplete (100-199)
  const teamInput = document.querySelector('input[name="assigned_to"]');
  if (teamInput) {
    // Create datalist for team numbers
    const datalist = document.createElement('datalist');
    datalist.id = 'team-numbers';
    for (let i = 100; i <= 199; i++) {
      const option = document.createElement('option');
      option.value = 'Team ' + i;
      datalist.appendChild(option);
    }
    document.body.appendChild(datalist);
    teamInput.setAttribute('list', 'team-numbers');
    teamInput.placeholder = 'Team 100-199';
  }
  
  // Facility filter based on site selection
  const siteSelect = document.getElementById('site-select');
  const facilitySelect = document.getElementById('facility-select');
  
  if (siteSelect && facilitySelect) {
    siteSelect.addEventListener('change', function() {
      const selectedSite = this.value;
      const options = facilitySelect.querySelectorAll('option');
      
      options.forEach(function(option) {
        if (!option.value) return; // Skip placeholder
        const optionSite = option.getAttribute('data-site');
        option.style.display = (optionSite === selectedSite || !selectedSite) ? '' : 'none';
      });
      
      // Reset facility selection if current is hidden
      if (facilitySelect.selectedOptions[0]?.style.display === 'none') {
        facilitySelect.value = '';
      }
    });
  }
  
  // Form submission handling for API calls
  document.querySelectorAll('form[action^="/api/"]').forEach(function(form) {
    form.addEventListener('submit', function(e) {
      e.preventDefault();
      
      const formData = new FormData(form);
      const action = form.getAttribute('action');
      
      fetch(action, {
        method: 'POST',
        body: formData
      })
      .then(response => response.json())
      .then(data => {
        if (data.status === 'created' || data.status === 'updated') {
          // Redirect based on context
          if (action.includes('work-orders/create')) {
            window.location.href = '/work-orders/' + data.id;
          } else if (action.includes('work-orders/update')) {
            window.location.reload();
          } else if (action.includes('sites/create')) {
            window.location.reload();
          } else {
            window.location.reload();
          }
        } else {
          alert('Operation completed: ' + JSON.stringify(data));
        }
      })
      .catch(error => {
        console.error('Error:', error);
        alert('An error occurred. Please try again.');
      });
    });
  });
  
  // Auto-submit filter forms on select change
  document.querySelectorAll('.filter-form select').forEach(function(select) {
    if (!select.hasAttribute('onchange')) {
      select.addEventListener('change', function() {
        // Don't auto-submit, let user click filter button
      });
    }
  });
  
  // Confirm before closing work orders
  const statusSelect = document.querySelector('select[name="status"]');
  if (statusSelect) {
    const form = statusSelect.closest('form');
    if (form) {
      form.addEventListener('submit', function(e) {
        const newStatus = statusSelect.value;
        if (newStatus === 'Closed') {
          if (!confirm('Are you sure you want to close this work order? This action should only be done after QAQC verification.')) {
            e.preventDefault();
          }
        }
      });
    }
  }
});
